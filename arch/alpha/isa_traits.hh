/*
 * Copyright (c) 2003 The Regents of The University of Michigan
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met: redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer;
 * redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution;
 * neither the name of the copyright holders nor the names of its
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef __ISA_TRAITS_HH__
#define __ISA_TRAITS_HH__

#include "sim/host.hh"
#include "targetarch/faults.hh"
#include "base/misc.hh"

class FullCPU;
class IniFile;

#define TARGET_ALPHA

template <class ISA> class StaticInst;
template <class ISA> class StaticInstPtr;

class AlphaISA
{
  public:

    typedef uint32_t MachInst;
    typedef uint64_t Addr;
    typedef uint8_t  RegIndex;

    enum {
        MemoryEnd = 0xffffffffffffffffULL,

        NumIntRegs = 32,
        NumFloatRegs = 32,
        NumMiscRegs = 32,

        MaxRegsOfAnyType = 32,
        // Static instruction parameters
        MaxInstSrcRegs = 3,
        MaxInstDestRegs = 2,

        // semantically meaningful register indices
        ZeroReg = 31,	// architecturally meaningful
        // the rest of these depend on the ABI
        StackPointerReg = 30,
        GlobalPointerReg = 29,
        ReturnAddressReg = 26,
        ReturnValueReg = 0,
        ArgumentReg0 = 16,
        ArgumentReg1 = 17,
        ArgumentReg2 = 18,
        ArgumentReg3 = 19,
        ArgumentReg4 = 20,
        ArgumentReg5 = 21,

        LogVMPageSize = 13,	// 8K bytes
        VMPageSize = (1 << LogVMPageSize),

        BranchPredAddrShiftAmt = 2, // instructions are 4-byte aligned

        WordBytes = 4,
        HalfwordBytes = 2,
        ByteBytes = 1,
        DepNA = 0,
    };

    // These enumerate all the registers for dependence tracking.
    enum DependenceTags {
        // 0..31 are the integer regs 0..31
        // 32..63 are the FP regs 0..31, i.e. use (reg + FP_Base_DepTag)
        FP_Base_DepTag = 32,
        Ctrl_Base_DepTag = 64,
        Fpcr_DepTag = 64,		// floating point control register
        Uniq_DepTag = 65,
        IPR_Base_DepTag = 66
    };

    typedef uint64_t IntReg;
    typedef IntReg IntRegFile[NumIntRegs];

    // floating point register file entry type
    typedef union {
        uint64_t q;
        double d;
    } FloatReg;

    typedef union {
        uint64_t q[NumFloatRegs];	// integer qword view
        double d[NumFloatRegs];		// double-precision floating point view
    } FloatRegFile;

    // control register file contents
    typedef uint64_t MiscReg;
    typedef struct {
        uint64_t	fpcr;		// floating point condition codes
        uint64_t	uniq;		// process-unique register
        bool		lock_flag;	// lock flag for LL/SC
        Addr		lock_addr;	// lock address for LL/SC
    } MiscRegFile;

#ifdef FULL_SYSTEM

    typedef uint64_t InternalProcReg;

#include "targetarch/isa_fullsys_traits.hh"

#else
    enum {
        NumInternalProcRegs = 0
    };
#endif

    enum {
        TotalNumRegs =
        NumIntRegs + NumFloatRegs + NumMiscRegs + NumInternalProcRegs
    };

    typedef union {
        IntReg  intreg;
        FloatReg   fpreg;
        MiscReg ctrlreg;
    } AnyReg;

    struct RegFile {
        IntRegFile intRegFile;		// (signed) integer register file
        FloatRegFile floatRegFile;	// floating point register file
        MiscRegFile miscRegs;		// control register file
        Addr pc;			// program counter
        Addr npc;			// next-cycle program counter
#ifdef FULL_SYSTEM
        IntReg palregs[NumIntRegs];	// PAL shadow registers
        InternalProcReg ipr[NumInternalProcRegs]; // internal processor regs
        int intrlock;			// interrupt register lock flag
        int intrflag;			// interrupt flag
        bool pal_shadow;		// using pal_shadow registers
#endif // FULL_SYSTEM
        // Are these architectural, or just for convenience?
        uint8_t opcode, ra;		// current instruction details (for intr's)
    };

    static StaticInstPtr<AlphaISA> decodeInst(MachInst);

    enum annotes {
        ANNOTE_NONE = 0,
        // An impossible number for instruction annotations
        ITOUCH_ANNOTE = 0xffffffff,
    };

    static inline bool isCallerSaveIntegerRegister(unsigned int reg) {
        panic("register classification not implemented");
        return (reg >= 1 && reg <= 8 || reg >= 22 && reg <= 25 || reg == 27);
    }

    static inline bool isCalleeSaveIntegerRegister(unsigned int reg) {
        panic("register classification not implemented");
        return (reg >= 9 && reg <= 15);
    }

    static inline bool isCallerSaveFloatRegister(unsigned int reg) {
        panic("register classification not implemented");
        return false;
    }

    static inline bool isCalleeSaveFloatRegister(unsigned int reg) {
        panic("register classification not implemented");
        return false;
    }

    static inline Addr alignAddress(const Addr &addr,
                                         unsigned int nbytes) {
        return (addr & ~(nbytes - 1));
    }

    // Instruction address compression hooks
    static inline Addr realPCToFetchPC(const Addr &addr) {
        return addr;
    }

    static inline Addr fetchPCToRealPC(const Addr &addr) {
        return addr;
    }

    // the size of "fetched" instructions (not necessarily the size
    // of real instructions for PISA)
    static inline size_t fetchInstSize() {
        return sizeof(MachInst);
    }

    static inline MachInst makeRegisterCopy(int dest, int src) {
        panic("makeRegisterCopy not implemented");
        return 0;
    }

    // Machine operations

    static void saveMachineReg(AnyReg &savereg, const RegFile &reg_file,
                               int regnum);

    static void restoreMachineReg(RegFile &regs, const AnyReg &reg,
                                  int regnum);

#if 0
    static void serializeSpecialRegs(const Serializeable::Proxy &proxy,
                                     const RegFile &regs);

    static void unserializeSpecialRegs(const IniFile *db,
                                       const std::string &category,
                                       ConfigNode *node,
                                       RegFile &regs);
#endif
};


typedef AlphaISA TheISA;

typedef TheISA::MachInst MachInst;
typedef TheISA::Addr Addr;
typedef TheISA::RegIndex RegIndex;
typedef TheISA::IntReg IntReg;
typedef TheISA::IntRegFile IntRegFile;
typedef TheISA::FloatReg FloatReg;
typedef TheISA::FloatRegFile FloatRegFile;
typedef TheISA::MiscReg MiscReg;
typedef TheISA::MiscRegFile MiscRegFile;
typedef TheISA::AnyReg AnyReg;
typedef TheISA::RegFile RegFile;

const int NumIntRegs   = TheISA::NumIntRegs;
const int NumFloatRegs = TheISA::NumFloatRegs;
const int NumMiscRegs  = TheISA::NumMiscRegs;
const int TotalNumRegs = TheISA::TotalNumRegs;
const int VMPageSize   = TheISA::VMPageSize;
const int LogVMPageSize   = TheISA::LogVMPageSize;
const int ZeroReg = TheISA::ZeroReg;
const int StackPointerReg = TheISA::StackPointerReg;
const int GlobalPointerReg = TheISA::GlobalPointerReg;
const int ReturnAddressReg = TheISA::ReturnAddressReg;
const int ReturnValueReg = TheISA::ReturnValueReg;
const int ArgumentReg0 = TheISA::ArgumentReg0;
const int ArgumentReg1 = TheISA::ArgumentReg1;
const int BranchPredAddrShiftAmt = TheISA::BranchPredAddrShiftAmt;

#ifdef FULL_SYSTEM
typedef TheISA::InternalProcReg InternalProcReg;
const int NumInternalProcRegs  = TheISA::NumInternalProcRegs;
const int NumInterruptLevels = TheISA::NumInterruptLevels;

// more stuff that should be imported here, but I'm too tired to do it
// right now...
#include "targetarch/ev5.hh"
#endif

#endif // __ALPHA_ISA_H__
