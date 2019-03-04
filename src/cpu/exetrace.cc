/*
 * Copyright (c) 2001-2005 The Regents of The University of Michigan
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
 *
 * Authors: Steve Reinhardt
 *          Lisa Hsu
 *          Nathan Binkert
 *          Steve Raasch
 */

#include <iomanip>

#include "arch/isa_traits.hh"
#include "arch/utility.hh"
#include "base/loader/symtab.hh"
#include "config/the_isa.hh"
#include "cpu/base.hh"
#include "cpu/exetrace.hh"
#include "cpu/static_inst.hh"
#include "cpu/thread_context.hh"
#include "debug/ExecAll.hh"
#include "enums/OpClass.hh"
#include "debug/zdcDetection.hh"
#include "debug/zdcProfile.hh"
#include "debug/FIinformation.hh"

using namespace std;
using namespace TheISA;

namespace Trace {

Tick lastTickForBl=0;
bool afterBl[4]={false, false, false, false};
bool isFirstInst[4]={true, true, true, true};

void
ExeTracerRecord::dumpTicks(ostream &outs)
{
    ccprintf(outs, "%7d: ", when);
}

void
Trace::ExeTracerRecord::traceInst(const StaticInstPtr &inst, bool ran)
{
    ostream &outs = Trace::output();
//moslem
    std::string sym_str,funcName;
    Addr sym_addr;
    Addr cur_pc = pc.instAddr();
debugSymbolTable->findNearestSymbol(cur_pc, funcName, sym_addr);
if ( ((funcName == "main" || ((funcName[0] == 'F' && funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C'))) ) )
{	
	if (inst->disassemble(cur_pc, debugSymbolTable).compare("  mov   lr, pc\0")==0 && thread->cpuId()==0)
		DPRINTF(FIinformation, "cpu%d/NORMAL END\n",thread->cpuId());



	if (inst->disassemble(cur_pc, debugSymbolTable).compare("  add   r8, r8, r8\0")==0)
		DPRINTF(zdcDetection, "cpu%d/ERRRORRRRRRRRRRR IS DETECTED on STORE\n",thread->cpuId());
	else if (inst->disassemble(cur_pc, debugSymbolTable).compare("  addne   r8, r8, r8\0")==0 && data_status != DataInvalid)
		DPRINTF(zdcDetection, "cpu%d/ERRRORRRRRRRRRRR IS DETECTED on STORE\n",thread->cpuId());
	else if (inst->disassemble(cur_pc, debugSymbolTable).compare("  sub   r8, r8, r8\0")==0)
		DPRINTF(zdcDetection, "cpu%d/ERRRORRRRRRRRRRR IS DETECTED on LIBCALL\n",thread->cpuId());
	else if (inst->disassemble(cur_pc, debugSymbolTable).compare("  mov   r8, r8\0")==0 && data.as_int == 4 )
		DPRINTF(zdcDetection, "cpu%d/MISMATCH IS DETECTED on silent check voting (False)\n",thread->cpuId());
	else if (inst->disassemble(cur_pc, debugSymbolTable).compare("  mov   r8, r8\0")==0 && data.as_int == 5 )
		DPRINTF(zdcDetection, "cpu%d/MISMATCH IS DETECTED on silent check voting (True)\n",thread->cpuId());
	else if (inst->disassemble(cur_pc, debugSymbolTable).compare("  movlt   r8, r8\0")==0 )
			DPRINTF(zdcDetection, "cpu%d/Unrecoverable Signal #1 (Not Silent, Stored data is same to backup\n",thread->cpuId());
	else if (inst->disassemble(cur_pc, debugSymbolTable).compare("  moveq   r8, r8\0")==0 && data_status != DataInvalid )
			DPRINTF(zdcDetection, "cpu%d/Unrecoverable Signal #2 (Not Silent, Stored data is same to backup and addr register is clean\n",thread->cpuId());
	else if (inst->disassemble(cur_pc, debugSymbolTable).compare("  movne   r8, r8\0")==0 && data_status != DataInvalid )
			DPRINTF(zdcDetection, "cpu%d/Unrecoverable Signal #3 Master control flow error\n",thread->cpuId());
		
	else if (inst->disassemble(cur_pc, debugSymbolTable).compare("  subne   r8, r8, r8\0")==0 && data_status != DataInvalid)
		DPRINTF(zdcDetection, "cpu%d/PERMANENT ERROR!\n",thread->cpuId());
	else if (inst->disassemble(cur_pc, debugSymbolTable).compare("  subsne   r8, r8, r8\0")==0 && data_status != DataInvalid)
		DPRINTF(zdcDetection, "cpu2/PERMANENT ERROR!\n");
	//HwiSoo
	if (afterBl[thread->cpuId()])
	{
		DPRINTF(zdcProfile, "cpu%d/blCallAfter/afterTick/%llu\n", thread->cpuId(), curTick());
		afterBl[thread->cpuId()] = false;
	}
	
	//note. not else if!
	if (inst->disassemble(cur_pc, debugSymbolTable).compare("  mov   r4, r4")==0)
	{
		DPRINTF(zdcProfile, "cpu%d/NRStart/currentTick/%llu\n", thread->cpuId(), curTick());
		thread->insideNonRepeatable=true;
	}
	else if (inst->disassemble(cur_pc, debugSymbolTable).compare("  mov   r7, r7")==0)
	{
		DPRINTF(zdcProfile, "cpu%d/NREnd/currentTick/%llu\n", thread->cpuId(), curTick());
		thread->insideNonRepeatable=false;
	}
	else if (inst->disassemble(cur_pc, debugSymbolTable).compare("  bl   ")==0)
	{
		DPRINTF(zdcProfile, "cpu%d/blCall/lastTick/%llu\n", thread->cpuId(), lastTickForBl);
		afterBl[thread->cpuId()] = true;
	}
	else if (isFirstInst[thread->cpuId()])
	{
		DPRINTF(zdcProfile, "cpu%d/threadStart/currentTick/%llu\n", thread->cpuId(), curTick());
		isFirstInst[thread->cpuId()]=false;
	}
	//Note: end of thread will be printed bellow 
	//HwiSoo
	else if (inst->disassemble(cur_pc, debugSymbolTable).find("  uopReg_uop   pc,")!=-1)
	{
		DPRINTF(zdcProfile, "cpu%d/threadEnd/currentTick/%llu\n", thread->cpuId(), curTick());
	}
	
	lastTickForBl=curTick();
	//HwiSoo End
    if (!Debug::ExecUser || !Debug::ExecKernel) {
        bool in_user_mode = TheISA::inUserMode(thread);
        if (in_user_mode && !Debug::ExecUser) return;
        if (!in_user_mode && !Debug::ExecKernel) return;
    }

    if (Debug::ExecTicks)
        dumpTicks(outs);

    outs << thread->getCpuPtr()->name() << " ";

    if (Debug::ExecAsid)
        outs << "A" << dec << TheISA::getExecutingAsid(thread) << " ";

    if (Debug::ExecThread)
        outs << "T" << thread->threadId() << " : ";

    std::string sym_str;
    Addr sym_addr;
    Addr cur_pc = pc.instAddr();
    if (debugSymbolTable && Debug::ExecSymbol &&
            (!FullSystem || !inUserMode(thread)) &&
            debugSymbolTable->findNearestSymbol(cur_pc, sym_str, sym_addr)) {
        if (cur_pc != sym_addr)
            sym_str += csprintf("+%d",cur_pc - sym_addr);
        outs << "@" << sym_str;
    } else {
        outs << "0x" << hex << cur_pc;
    }

    if (inst->isMicroop()) {
        outs << "." << setw(2) << dec << pc.microPC();
    } else {
        outs << "   ";
    }

    outs << " : ";

    //
    //  Print decoded instruction
    //


    outs << setw(26) << left;
    outs << inst->disassemble(cur_pc, debugSymbolTable);
	
    if (ran) {
        outs << " : ";

        if (Debug::ExecOpClass) {
            outs << Enums::OpClassStrings[inst->opClass()] << " : ";
        }

        if (Debug::ExecResult && !predicate) {
            outs << "Predicated False";
        }

        if (Debug::ExecResult && data_status != DataInvalid) {
            ccprintf(outs, " D=%#018x", data.as_int);
        }

        if (Debug::ExecEffAddr && getMemValid())
            outs << " A=0x" << hex << addr;

        if (Debug::ExecFetchSeq && fetch_seq_valid)
            outs << "  FetchSeq=" << dec << fetch_seq;

        if (Debug::ExecCPSeq && cp_seq_valid)
            outs << "  CPSeq=" << dec << cp_seq;

        if (Debug::ExecFlags) {
            outs << "  flags=(";
            inst->printFlags(outs, "|");
            outs << ")";
        }
    }

    //
    //  End of line...
    //
    outs << endl;
}
}
void
Trace::ExeTracerRecord::dump()
{
    /*
     * The behavior this check tries to achieve is that if ExecMacro is on,
     * the macroop will be printed. If it's on and microops are also on, it's
     * printed before the microops start printing to give context. If the
     * microops aren't printed, then it's printed only when the final microop
     * finishes. Macroops then behave like regular instructions and don't
     * complete/print when they fault.
     */
    if (Debug::ExecMacro && staticInst->isMicroop() &&
        ((Debug::ExecMicro &&
            macroStaticInst && staticInst->isFirstMicroop()) ||
            (!Debug::ExecMicro &&
             macroStaticInst && staticInst->isLastMicroop()))) {
        traceInst(macroStaticInst, false);
    }
    if (Debug::ExecMicro || !staticInst->isMicroop()) {
        traceInst(staticInst, true);
    }
}

} // namespace Trace

////////////////////////////////////////////////////////////////////////
//
//  ExeTracer Simulation Object
//
Trace::ExeTracer *
ExeTracerParams::create()
{
    return new Trace::ExeTracer(this);
}
