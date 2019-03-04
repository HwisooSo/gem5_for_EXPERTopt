/*
 * Copyright (c) 2011-2014 ARM Limited
 * Copyright (c) 2013 Advanced Micro Devices, Inc.
 * All rights reserved
 *
 * The license below extends only to copyright in the software and shall
 * not be construed as granting a license to any other intellectual
 * property including but not limited to intellectual property relating
 * to a hardware implementation of the functionality of the software
 * licensed hereunder.  You may use the software subject to the license
 * terms below provided that you ensure that this notice is replicated
 * unmodified and in its entirety in all distributions of the software,
 * modified or unmodified, in source code or in binary form.
 *
 * Copyright (c) 2002-2005 The Regents of The University of Michigan
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
 *          Dave Greene
 *          Nathan Binkert
 *          Andrew Bardsley
 */

/**
 * @file
 *
 *  ExecContext bears the exec_context interface for Minor.
 */

#ifndef __CPU_MINOR_EXEC_CONTEXT_HH__
#define __CPU_MINOR_EXEC_CONTEXT_HH__

#include "cpu/exec_context.hh"
#include "cpu/minor/execute.hh"
#include "cpu/minor/pipeline.hh"
#include "cpu/base.hh"
#include "cpu/simple_thread.hh"
#include "debug/MinorExecute.hh"
#include "debug/registerAccesses.hh"
#include "debug/MemAccesses.hh"
#include "debug/faultInjectionTrack.hh"
#include "debug/CCAccesses.hh"
#include "debug/silentnessProfile.hh"
namespace Minor
{

/* Forward declaration of Execute */
class Execute;

/** ExecContext bears the exec_context interface for Minor.  This nicely
 *  separates that interface from other classes such as Pipeline, MinorCPU
 *  and DynMinorInst and makes it easier to see what state is accessed by it.
 */
class ExecContext : public ::ExecContext
{
  public:
    MinorCPU &cpu;

    /** ThreadState object, provides all the architectural state. */
    SimpleThread &thread;

    /** The execute stage so we can peek at its contents. */
    Execute &execute;

    /** Instruction for the benefit of memory operations and for PC */
    MinorDynInstPtr inst;

	//HWISOO
	InstSeqNum previousSeqNum=0;
	int operandCountInSameSeq=-1;
	InstSeqNum setPreviousSeqNum=0;
	int setOperandCountInSameSeq=-1;
	
	unsigned int lsq_inst_count=0;
	InstSeqNum prevTargetLSQSeqNum=0;
	
    ExecContext (
        MinorCPU &cpu_,
        SimpleThread &thread_, Execute &execute_,
        MinorDynInstPtr inst_) :
        cpu(cpu_),
        thread(thread_),
        execute(execute_),
        inst(inst_)
    {
        DPRINTF(MinorExecute, "ExecContext setting PC: %s\n", inst->pc);
        pcState(inst->pc);
        setPredicate(true);
        thread.setIntReg(TheISA::ZeroReg, 0);
#if THE_ISA == ALPHA_ISA
        thread.setFloatReg(TheISA::ZeroReg, 0.0);
#endif
    }

    Fault
    readMem(Addr addr, uint8_t *data, unsigned int size,
        unsigned int flags)
    {
		std::string funcName="nothing";
		Addr sym_addr;
		debugSymbolTable->findNearestSymbol(cpu.getContext(0)->instAddr(), funcName, sym_addr);
			
		if( (funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") )
			DPRINTF(MemAccesses, "cpu%d/%s/%d/readMem/0x%x/%d/Test_opClass:%s/FU=%d\n", cpu.cpuId(), inst->staticInst->disassemble(0), inst->id.execSeqNum, addr, size, Enums::OpClassStrings[inst->staticInst->opClass()],inst->fuIndex);
			//HWISOO
		
		/*
		if ( (cpu.cpuId() == execute.FIcore) && (!execute.faultIsInjected) && (execute.oldFIcomponent == 6) && (curTick() == execute.FItick) && (execute.FIentry == inst->id.execSeqNum))
		{
			if(execute.FIbit<128) // data
			{
				
				int injectByte = execute.FIbit / 8;
				uint8_t injectBit = pow (2, execute.FIbit %8);
				uint8_t dataByte = data[injectByte];
				DPRINTF(faultInjectionTrack, "************Fault is activating**************\nCPU:%d:In function %s, instruction %s original data[%d] is %d\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), injectByte, data[injectByte]);
				dataByte = dataByte xor injectBit;
				data[injectByte] = dataByte;
				DPRINTF(faultInjectionTrack, "************Fault is activated**************\nCPU:%d:In function %s, instruction %s data[%d] is now %d\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), injectByte, data[injectByte]);
				
				//HWISOO. load data will be changed in set~~
			}
			else // addr
			{
				Addr originalAddr = addr;
				Addr injectBit = pow (2, (execute.FIbit-128));
				addr = addr xor injectBit;
				DPRINTF(faultInjectionTrack, "************Fault is activated**************\nCPU:%d:In function %s, instruction %s addr is 0x%x -> 0x%x\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), originalAddr, addr);
				execute.faultIsInjected=true;
			}
		}
		*/
		
		//HWISOO. temporal to LSQ
		if ( (cpu.cpuId() == execute.FIcore) && (!execute.faultIsInjected) && (execute.FIcomponent == 10) && (curTick() == execute.FItick) && (execute.FIentry == inst->id.execSeqNum))
		{
			if(execute.FIbit<256) // data
			{
				
				/*
				int injectByte = execute.FIbit / 8;
				uint8_t injectBit = pow (2, execute.FIbit %8);
				uint8_t dataByte = data[injectByte];
				DPRINTF(faultInjectionTrack, "************Fault is activating**************\nCPU:%d:In function %s, instruction %s original data[%d] is %d\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), injectByte, data[injectByte]);
				dataByte = dataByte xor injectBit;
				data[injectByte] = dataByte;
				DPRINTF(faultInjectionTrack, "************Fault is activated**************\nCPU:%d:In function %s, instruction %s data[%d] is now %d\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), injectByte, data[injectByte]);
				
				*/
				
				
				//HWISOO. load data will be changed in set~~
			}
			else if (/*execute.FIbit>=256 && */execute.FIbit<320) // addr
			{
				Addr originalAddr = addr;
				Addr injectBit = pow (2, (execute.FIbit-256));
				addr = addr xor injectBit;
				DPRINTF(faultInjectionTrack, "************Fault is activated**************\nCPU:%d:In function %s, instruction %s addr is 0x%x -> 0x%x\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), originalAddr, addr);
				execute.faultIsInjected=true;
			}
			else if(/* execute.FIbit>=320 && */ execute.FIbit<324)
			{
				unsigned int originalSize = size;
				//unsigned int injectBit = pow (2, (execute.FIbit-320));
				unsigned int injectBit = (execute.FIbit-320);
				//size = size xor injectBit;
				size = (size<<injectBit)%32;
				
				DPRINTF(faultInjectionTrack, "************Fault is activated**************\nCPU:%d:In function %s, instruction %s size is %d -> %d\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), originalSize, size);
				execute.faultIsInjected=true;
				
			}
		}
		
		//permanent to LSQ
		if ( (cpu.cpuId() == execute.FIcore) && (execute.FIcomponent == 20) && (execute.FIentry==lsq_inst_count))
		{
			if( ( (funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") ) && curTick() >= execute.FItick && cpu.getContext(inst->id.threadId)->insideNonRepeatable == false)
			{
				if(execute.FIbit<256) // data
				{
					prevTargetLSQSeqNum	= inst->id.execSeqNum;
				}
				else if (/*execute.FIbit>=256 && */execute.FIbit<320) // addr
				{
					if(execute.FIbit < 256+32) //makes 0
					{
						Addr injectBit = pow (2, (execute.FIbit-256));
						injectBit = ~injectBit;
						addr = addr & injectBit;
					}
					else //makes 1
					{
						Addr injectBit = pow (2, (execute.FIbit-256-32));
						addr = addr | injectBit;					
					}
				}
				else if(/* execute.FIbit>=320 && */ execute.FIbit<325)
				{
					size = 1<<(execute.FIbit-320);
					
				}
			}
		}
		
		//permanent
		/*
		if ( (cpu.cpuId() == execute.FIcore) && (execute.oldFIcomponent == 15) && (execute.FIentry==lsq_inst_count))
		{
			if( ( (funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") ) && curTick() >= execute.FItick && cpu.getContext(inst->id.threadId)->insideNonRepeatable == false)
			{
				if(execute.FIbit<256) // data
				{
					prevTargetLSQSeqNum	= inst->id.execSeqNum;
				}
				else // addr
				{
					if(execute.FIbit < 256+32) //makes 0
					{
						Addr injectBit = pow (2, (execute.FIbit-256));
						injectBit = ~injectBit;
						addr = addr & injectBit;
					}
					else //makes 1
					{
						Addr injectBit = pow (2, (execute.FIbit-256-32));
						addr = addr | injectBit;					
					}
				}
			}
		}
		
		*/
		
		
		execute.getLSQ().pushRequest(inst, true /* load */, data,
			size, addr, flags, NULL);
			
		lsq_inst_count=(lsq_inst_count+1)%5;//HWISOO. 5-entry LSQ
        return NoFault;
		

    }

    Fault
    writeMem(uint8_t *data, unsigned int size, Addr addr,
        unsigned int flags, uint64_t *res)
    {
		std::string funcName="nothing";
		Addr sym_addr;
		debugSymbolTable->findNearestSymbol(cpu.getContext(0)->instAddr(), funcName, sym_addr);
			
		if( (funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") )			
		DPRINTF(MemAccesses, "cpu%d/%s/%d/writeMem/0x%x/%d/Test_opClass:%s/FU=%d\n", cpu.cpuId(), inst->staticInst->disassemble(0), inst->id.execSeqNum, addr, size, Enums::OpClassStrings[inst->staticInst->opClass()],inst->fuIndex);
		
		/*
		if ( (cpu.cpuId() == execute.FIcore) && (!execute.faultIsInjected) && (execute.oldFIcomponent == 6) && (curTick() == execute.FItick) && (execute.FIentry == inst->id.execSeqNum) && (execute.FIbit>=256))
		{
			if(execute.FIbit<384) // data
			{
				int injectByte = (execute.FIbit-256) / 8;
				uint8_t injectBit = pow (2, (execute.FIbit-256) %8);
				uint8_t dataByte = data[injectByte];
				DPRINTF(faultInjectionTrack, "************Fault is activating**************\nCPU:%d:In function %s, instruction %s original data[%d] is %d\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), injectByte, data[injectByte]);
				dataByte = dataByte xor injectBit;
				data[injectByte] = dataByte;
				DPRINTF(faultInjectionTrack, "************Fault is activated**************\nCPU:%d:In function %s, instruction %s data[%d] is now %d\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), injectByte, data[injectByte]);
				execute.faultIsInjected=true;
			}
			else // addr
			{
				Addr originalAddr = addr;
				Addr injectBit = pow (2, (execute.FIbit-384));
				addr = addr xor injectBit;
				DPRINTF(faultInjectionTrack, "************Fault is activated**************\nCPU:%d:In function %s, instruction %s addr is 0x%x -> 0x%x\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), originalAddr, addr);
				execute.faultIsInjected=true;
			}
		}	
		*/
		
		
		//HWISOO. temporal to LSQ
		if ( (cpu.cpuId() == execute.FIcore) && (!execute.faultIsInjected) && (execute.FIcomponent == 10) && (curTick() == execute.FItick) && (execute.FIentry == inst->id.execSeqNum))
		{
			if(execute.FIbit<256) // data
			{
				int injectByte = (execute.FIbit) / 8;
				uint8_t injectBit = pow (2, (execute.FIbit) %8);
				uint8_t dataByte = data[injectByte];
				DPRINTF(faultInjectionTrack, "************Fault is activating**************\nCPU:%d:In function %s, instruction %s original data[%d] is %d\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), injectByte, data[injectByte]);
				dataByte = dataByte xor injectBit;
				data[injectByte] = dataByte;
				DPRINTF(faultInjectionTrack, "************Fault is activated**************\nCPU:%d:In function %s, instruction %s data[%d] is now %d\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), injectByte, data[injectByte]);
				execute.faultIsInjected=true;
			}
			else if (/*execute.FIbit>=256 && */execute.FIbit<320) // addr
			{
				Addr originalAddr = addr;
				Addr injectBit = pow (2, (execute.FIbit-256));
				addr = addr xor injectBit;
				DPRINTF(faultInjectionTrack, "************Fault is activated**************\nCPU:%d:In function %s, instruction %s addr is 0x%x -> 0x%x\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), originalAddr, addr);
				execute.faultIsInjected=true;
			}
			else if(/* execute.FIbit>=320 && */ execute.FIbit<324)
			{
				unsigned int originalSize = size;
				//unsigned int injectBit = pow (2, (execute.FIbit-320));
				unsigned int injectBit = (execute.FIbit-320);
				//size = size xor injectBit;
				size = (size<<injectBit)%32;
				
				DPRINTF(faultInjectionTrack, "************Fault is activated**************\nCPU:%d:In function %s, instruction %s size is %d -> %d\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), originalSize, size);
				execute.faultIsInjected=true;
				
			}
		}	
	
		/*
		if ( (cpu.cpuId() == execute.FIcore) && (execute.oldFIcomponent == 15) && (execute.FIentry==lsq_inst_count))
		{
			if( ( (funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") ) && curTick() >= execute.FItick && cpu.getContext(inst->id.threadId)->insideNonRepeatable == false)
			{
				if(execute.FIbit<256) // data
				{
					if(execute.FIbit < 128) // makes 0
					{
						int injectByte = (execute.FIbit) / 8;
						uint8_t injectBit = pow (2, (execute.FIbit) %8);
						if(size > injectByte)
						{
							uint8_t dataByte = data[injectByte];

							dataByte = dataByte & (~injectBit);
							data[injectByte] = dataByte;
						}
					}
					else // maeks 1
					{
						int injectByte = (execute.FIbit-128) / 8;
						uint8_t injectBit = pow (2, (execute.FIbit-128) %8);
						if(size > injectByte)
						{
							uint8_t dataByte = data[injectByte];

							dataByte = dataByte | injectBit;
							data[injectByte] = dataByte;
						}						
					}
				}
				else // addr
				{
					if(execute.FIbit<256+32)
					{
						Addr injectBit = pow (2, (execute.FIbit-256));
						injectBit = ~injectBit;
						addr = addr & injectBit;						
					}
					else
					{
						Addr injectBit = pow (2, (execute.FIbit-256-32));
						addr = addr | injectBit;
					}


				}
			}
		}	
		
		*/
		
		//permanent to LSQ
		if ( (cpu.cpuId() == execute.FIcore) && (execute.FIcomponent == 20) && (execute.FIentry==lsq_inst_count))
		{
			if( ( (funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") ) && curTick() >= execute.FItick && cpu.getContext(inst->id.threadId)->insideNonRepeatable == false)
			{
				if(execute.FIbit<256) // data
				{
					if(execute.FIbit < 128) // makes 0
					{
						int injectByte = (execute.FIbit) / 8;
						uint8_t injectBit = pow (2, (execute.FIbit) %8);
						if(size > injectByte)
						{
							uint8_t dataByte = data[injectByte];

							dataByte = dataByte & (~injectBit);
							data[injectByte] = dataByte;
						}
					}
					else // maeks 1
					{
						int injectByte = (execute.FIbit-128) / 8;
						uint8_t injectBit = pow (2, (execute.FIbit-128) %8);
						if(size > injectByte)
						{
							uint8_t dataByte = data[injectByte];

							dataByte = dataByte | injectBit;
							data[injectByte] = dataByte;
						}						
					}
				}
				else if (/*execute.FIbit>=256 && */execute.FIbit<320) // addr
				{
					if(execute.FIbit<256+32)
					{
						Addr injectBit = pow (2, (execute.FIbit-256));
						injectBit = ~injectBit;
						addr = addr & injectBit;						
					}
					else
					{
						Addr injectBit = pow (2, (execute.FIbit-256-32));
						addr = addr | injectBit;
					}
				}
				else if(/* execute.FIbit>=320 && */ execute.FIbit<325)
				{
					size = 1<<(execute.FIbit-320);
					
				}
			}
		}	
		
		execute.getLSQ().pushRequest(inst, false /* store */, data,
            size, addr, flags, res);
		lsq_inst_count=(lsq_inst_count+1)%5;//HWISOO. 5-entry LSQ
        return NoFault;
		

    }

    IntReg
    readIntRegOperand(const StaticInst *si, int idx)
    {
// moslem FI
        std::string funcName="nothing";
	Addr sym_addr;
	debugSymbolTable->findNearestSymbol(cpu.getContext(0)->instAddr(), funcName, sym_addr);
        if( (funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") ){
			if(si->srcRegIdx(idx)<16)
				DPRINTF(registerAccesses, "cpu%d/%s/%d/readInt/%d/%d/FU=%d/isLoad=%d\n", cpu.cpuId(), si->disassemble(0), inst->id.execSeqNum, si->srcRegIdx(idx), idx, inst->fuIndex, si->isLoad());
			//HWISOO
        }
        if(execute.FIcore == cpu.cpuId() && execute.FIcomponent == 1)
			if (execute.faultIsInjected && !execute.faultGetsMasked && !execute.faultGetsActivated && execute.FIentry == si->srcRegIdx(idx)){
				DPRINTF(faultInjectionTrack, "************Fault is activated**************\nCPU:%d:In function %s, instruction %s is reading register %d: Faulty value is %d\n", cpu.cpuId(), funcName, si->disassemble(0), si->srcRegIdx(idx), thread.readIntReg(si->srcRegIdx(idx)));
				execute.faultGetsActivated=true;
           }


		//HWISOO. temporal to register pointer
		if ( (cpu.cpuId() == execute.FIcore) && (!execute.faultIsInjected) && (execute.FIcomponent == 9) && (curTick() == execute.FItick) && (execute.FIentry == inst->id.execSeqNum) && (int(execute.FIbit / 8) == idx ))
		{
			int temp = pow (2, execute.FIbit % 8);
			int corruptedIdx = temp xor si->srcRegIdx(idx);
			execute.faultIsInjected=true;
			
		    DPRINTF(faultInjectionTrack, "************Fault is activated**************\nCPU:%d:In function %s, instruction %s is reading integer register (%d -> %d)\n", cpu.cpuId(), funcName, si->disassemble(0), si->srcRegIdx(idx), corruptedIdx);
			
			return thread.readIntReg(corruptedIdx);
		}

		//HWISOO. permanent to register pointer
		if ( (cpu.cpuId() == execute.FIcore) && (execute.FIcomponent == 19) && (execute.FIentry < 2))
		{
			if( ( (funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") ) && curTick() >= execute.FItick && cpu.getContext(inst->id.threadId)->insideNonRepeatable == false)
			{
				if(previousSeqNum!=inst->id.execSeqNum)
					operandCountInSameSeq=0;
				else
					operandCountInSameSeq++;
				previousSeqNum = inst->id.execSeqNum;
				
				if(execute.FIentry ==operandCountInSameSeq)
				{
					int corruptedIdx=si->srcRegIdx(idx);
					if(execute.FIbit <4)
					{
						int temp = pow (2, (execute.FIbit));				
						corruptedIdx=corruptedIdx&(~temp);
					}
					else
					{
						int temp = pow (2, (execute.FIbit-4));				
						corruptedIdx=corruptedIdx|temp;				
					}
					return thread.readIntReg(corruptedIdx);
				}
			}
		}
		
		//HWISOO. temporal to FU (mem (5) only) (as it does not have any kind of writeReg)
		if ( (cpu.cpuId() == execute.FIcore) && (execute.FIcomponent == 8) && (curTick() == execute.FItick)&& (execute.FIentry == inst->id.execSeqNum) && (int(execute.FIbit / 32) == idx) && (inst->fuIndex == 5) )
		{
			IntReg temp = pow (2, execute.FIbit % 32);
			IntReg originalValue = thread.readIntReg(si->srcRegIdx(idx));
			
			
			IntReg corruptedValue = temp xor originalValue;
			execute.faultIsInjected=true;
			
			DPRINTF(faultInjectionTrack, "************Fault is activated**************\nCPU:%d:In function %s, instruction %s data of FU %d is now %d\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), inst->fuIndex, corruptedValue);
			
			return corruptedValue;
			
		}
		
		//HWISOO. permanent to FU (mem (5) only) (as it does not have any kind of writeReg)
		if ( (cpu.cpuId() == execute.FIcore) && (execute.FIcomponent == 18) && curTick() >= execute.FItick && !si->isLoad())
		{
			if( ( (funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") ) && cpu.getContext(inst->id.threadId)->insideNonRepeatable == false && inst->fuIndex == 5  && execute.FIentry>=2)
			{
				if(previousSeqNum!=inst->id.execSeqNum)
					operandCountInSameSeq=0;
				else
					operandCountInSameSeq++;
				previousSeqNum = inst->id.execSeqNum;
				
				if((execute.FIentry-2) ==operandCountInSameSeq)
				{
					
					IntReg originalValue = thread.readIntReg(si->srcRegIdx(idx));
					IntReg corruptedValue=originalValue;
					IntReg injectBit;
					if (execute.FIbit<32) // makes 0
					{
						injectBit = (pow (2, execute.FIbit)); // ex) 1111101111111...
						injectBit = ~injectBit;
						corruptedValue = originalValue & injectBit;
					}
					else if (execute.FIbit>=32 && execute.FIbit<64)// makes 1
					{
						injectBit = (pow (2, execute.FIbit-32)); // ex) 0000010000000...
						corruptedValue = originalValue | injectBit;
					}

					return corruptedValue;
				}
			}
		}
		
		
		
		
		
        return thread.readIntReg(si->srcRegIdx(idx));

    }

    TheISA::FloatReg
    readFloatRegOperand(const StaticInst *si, int idx)
    {
		std::string funcName="nothing";
		Addr sym_addr;
		debugSymbolTable->findNearestSymbol(cpu.getContext(0)->instAddr(), funcName, sym_addr);
        int reg_idx = si->srcRegIdx(idx) - TheISA::FP_Reg_Base;
		
		if( (funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") )
			DPRINTF(registerAccesses, "cpu%d/%s/%d/readFloat/%d/%d/FU=%d/isLoad=%d\n", cpu.cpuId(), si->disassemble(0), inst->id.execSeqNum ,reg_idx, idx, inst->fuIndex, si->isLoad()); //HWISOO
		
		//HWISOO. temporal to register pointer
		if ( (cpu.cpuId() == execute.FIcore) && (!execute.faultIsInjected) && (execute.FIcomponent == 9) && (curTick() == execute.FItick) && (execute.FIentry == inst->id.execSeqNum)  && (int(execute.FIbit / 8) == idx ))
		{
			int temp = pow (2, execute.FIbit % 8);
			int corruptedIdx = temp xor reg_idx;
			execute.faultIsInjected=true;
			
		    DPRINTF(faultInjectionTrack, "************Fault is activated**************\nCPU:%d:In function %s, instruction %s is reading float register (%d -> %d)\n", cpu.cpuId(), funcName, si->disassemble(0), reg_idx, corruptedIdx);
			
			return thread.readFloatReg(corruptedIdx);
		}
		

		//HWISOO. permanent to register pointer
		if ( (cpu.cpuId() == execute.FIcore) && (execute.FIcomponent == 19) && (execute.FIentry < 2))
		{
			if( ( (funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") ) && curTick() >= execute.FItick && cpu.getContext(inst->id.threadId)->insideNonRepeatable == false)
			{
				if(previousSeqNum!=inst->id.execSeqNum)
					operandCountInSameSeq=0;
				else
					operandCountInSameSeq++;
				previousSeqNum = inst->id.execSeqNum;
				
				if(execute.FIentry ==operandCountInSameSeq)
				{
					int corruptedIdx=reg_idx;
					if(execute.FIbit <4)
					{
						int temp = pow (2, (execute.FIbit));				
						corruptedIdx=corruptedIdx&(~temp);
					}
					else
					{
						int temp = pow (2, (execute.FIbit-4));				
						corruptedIdx=corruptedIdx|temp;				
					}
					return thread.readFloatReg(corruptedIdx);
				}
			}
		}
		
		
		
        return thread.readFloatReg(reg_idx);
		
	
    }

    TheISA::FloatRegBits
    readFloatRegOperandBits(const StaticInst *si, int idx)
    {
		std::string funcName="nothing";
		Addr sym_addr;
		debugSymbolTable->findNearestSymbol(cpu.getContext(0)->instAddr(), funcName, sym_addr);

        int reg_idx = si->srcRegIdx(idx) - TheISA::FP_Reg_Base;
		
		if( (funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") )
			DPRINTF(registerAccesses, "cpu%d/%s/%d/readFloatBits/%d/%d/FU=%d/isLoad=%d\n", cpu.cpuId(), si->disassemble(0), inst->id.execSeqNum ,reg_idx,idx, inst->fuIndex, si->isLoad()); //HWISOO
		
		//HWISOO. temporal to register pointer
		if ( (cpu.cpuId() == execute.FIcore) && (!execute.faultIsInjected) && (execute.FIcomponent == 9) && (curTick() == execute.FItick) && (execute.FIentry == inst->id.execSeqNum) && (int(execute.FIbit / 8) == idx ))
		{
			int temp = pow (2, execute.FIbit % 8);
			int corruptedIdx = temp xor reg_idx;
			execute.faultIsInjected=true;
			
		    DPRINTF(faultInjectionTrack, "************Fault is activated**************\nCPU:%d:In function %s, instruction %s is reading float register (%d -> %d)\n", cpu.cpuId(), funcName, si->disassemble(0), reg_idx, corruptedIdx);
			
			return thread.readFloatRegBits(corruptedIdx);
		}
		
		//HWISOO. permanent to register pointer
		if ( (cpu.cpuId() == execute.FIcore) && (execute.FIcomponent == 19) && (execute.FIentry < 2))
		{
			if( ( (funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") ) && curTick() >= execute.FItick && cpu.getContext(inst->id.threadId)->insideNonRepeatable == false)
			{
				if(previousSeqNum!=inst->id.execSeqNum)
					operandCountInSameSeq=0;
				else
					operandCountInSameSeq++;
				previousSeqNum = inst->id.execSeqNum;
				
				if(execute.FIentry ==operandCountInSameSeq)
				{
					int corruptedIdx=reg_idx;
					if(execute.FIbit <4)
					{
						int temp = pow (2, (execute.FIbit));				
						corruptedIdx=corruptedIdx&(~temp);
					}
					else
					{
						int temp = pow (2, (execute.FIbit-4));				
						corruptedIdx=corruptedIdx|temp;				
					}
					return thread.readFloatRegBits(corruptedIdx);
				}
			}
		}
		
		
		
		//HWISOO. temporal to FU (mem (5) only) (as it does not have any kind of writeReg)
		if ( (cpu.cpuId() == execute.FIcore) && (execute.FIcomponent == 8) && (curTick() == execute.FItick)&& (execute.FIentry == inst->id.execSeqNum) && (int(execute.FIbit / 32) == idx) && (inst->fuIndex == 5) )
		{
			TheISA::FloatRegBits temp = pow (2, execute.FIbit % 32);
			TheISA::FloatRegBits originalValue = thread.readFloatRegBits(reg_idx);
			
			
			TheISA::FloatRegBits corruptedValue = temp xor originalValue;
			execute.faultIsInjected=true;
			
			DPRINTF(faultInjectionTrack, "************Fault is activated**************\nCPU:%d:In function %s, instruction %s data of FU %d is now %d\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), inst->fuIndex, corruptedValue);
			
			return corruptedValue;
			
		}
		
		//HWISOO. permanent to FU (mem (5) only) (as it does not have any kind of writeReg)
		if ( (cpu.cpuId() == execute.FIcore) && (execute.FIcomponent == 18 ) && curTick() >= execute.FItick && !si->isLoad())
		{
			if( ( (funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") ) && cpu.getContext(inst->id.threadId)->insideNonRepeatable == false && (inst->fuIndex == 5)  && execute.FIentry>=2 )
			{

				if(previousSeqNum!=inst->id.execSeqNum)
					operandCountInSameSeq=0;
				else
					operandCountInSameSeq++;
				previousSeqNum = inst->id.execSeqNum;
				
				if(execute.FIentry-2 ==operandCountInSameSeq)
				{
					
					TheISA::FloatRegBits originalValue = thread.readFloatRegBits(reg_idx);
					TheISA::FloatRegBits corruptedValue = originalValue;
					TheISA::FloatRegBits injectBit;
					if (execute.FIbit<32) // makes 0
					{
						injectBit = (pow (2, execute.FIbit)); // ex) 1111101111111...
						injectBit = ~injectBit;
						corruptedValue = originalValue & injectBit;
					}
					else if (execute.FIbit>=32 && execute.FIbit<64)// makes 1
					{
						injectBit = (pow (2, execute.FIbit-32)); // ex) 0000010000000...
						corruptedValue = originalValue | injectBit;
					}
					
					
					return corruptedValue;
				}
				
			}
		}
		
		
        return thread.readFloatRegBits(reg_idx);
		
    }

    void
    setIntRegOperand(const StaticInst *si, int idx, IntReg val)
    {
//moslem fault injection in RF
        std::string funcName="nothing";
	Addr sym_addr;
	debugSymbolTable->findNearestSymbol(cpu.getContext(0)->instAddr(), funcName, sym_addr);
	if( (funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") )
	{
		if(si->destRegIdx(idx)<16)
			DPRINTF(registerAccesses, "cpu%d/%s/%d/writeInt/%d/%d/FU=%d/isLoad=%d\n", cpu.cpuId(), si->disassemble(0), inst->id.execSeqNum ,si->destRegIdx(idx),idx, inst->fuIndex, si->isLoad()); //HWISOO
		if (thread.readIntReg(si->destRegIdx(idx))==val)
		{
			DPRINTF(silentnessProfile, "cpu%d/sile/int\n", cpu.cpuId()); //HWISOO
		}
		else
		{
			DPRINTF(silentnessProfile, "cpu%d/diff/int\n", cpu.cpuId()); //HWISOO
		}		
	}
//For FI tracking

	





	if(execute.FIcore == cpu.cpuId() && execute.FIcomponent == 1)
		if (execute.faultIsInjected && !execute.faultGetsActivated && !execute.faultGetsMasked && execute.FIentry == si->destRegIdx(idx)){
		  DPRINTF(faultInjectionTrack, "************Fault is Masked**************\nCPU:%d:In function %s, instruction %s is overwriting the faulty register %d: Faulty value was %d\n", cpu.cpuId(), funcName, si->disassemble(0), si->destRegIdx(idx), thread.readIntReg(si->destRegIdx(idx)));
			execute.faultGetsMasked=true;
		fatal("%s: ****************MASKED!!!*****************\n",curTick() );
	  }


		//HWISOO. temporal to register pointer
		if ( (cpu.cpuId() == execute.FIcore) && (!execute.faultIsInjected) && (execute.FIcomponent == 9) && (curTick() == execute.FItick) && (execute.FIentry == inst->id.execSeqNum) && (int((execute.FIbit) / 8) == idx ))
		{
			int temp = pow (2, (execute.FIbit) % 8);
			int corruptedIdx = temp xor si->destRegIdx(idx);
			execute.faultIsInjected=true;
			
		    DPRINTF(faultInjectionTrack, "************Fault is activated**************\nCPU:%d:In function %s, instruction %s is reading integer register (%d -> %d)\n", cpu.cpuId(), funcName, si->disassemble(0), si->destRegIdx(idx), corruptedIdx);
			
			thread.setIntReg(corruptedIdx, val);
			return;
		}

		//HWISOO. permanent injection (to LSQ)
		if ( (cpu.cpuId() == execute.FIcore) && (execute.FIcomponent == 20) && (execute.FIbit<256) && prevTargetLSQSeqNum == inst->id.execSeqNum)
		{
			if( ( (funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") ) && curTick() >= execute.FItick && cpu.getContext(inst->id.threadId)->insideNonRepeatable == false)
			{
				//HWISOO: we don't need it as we have prevTargetLSQSeqNum
				//if ( ( ((int)(si->disassemble(0).find(" ld"))) >=0 ) or ( ((int)(si->disassemble(0).find(" vld"))) >=0 ) )
				{
					if (execute.FIbit<128) //makes 0
					{
						unsigned int injection_bit=((execute.FIbit)%32);
						unsigned int injection_idx=((execute.FIbit)/32);
						if (injection_idx==idx)
						{
							IntReg injectBit = pow (2, injection_bit);
							val = val & (~injectBit);
						}
					}
					
					else //makes 1
					{
						unsigned int injection_bit=((execute.FIbit-128)%32);
						unsigned int injection_idx=((execute.FIbit-128)/32);
						if (injection_idx==idx)
						{
							IntReg injectBit = pow (2, injection_bit);
							val = val | injectBit;
						}
					}
				}
			}
		}
		
		
		//HWISOO. temporal to LSQ
		if ( (cpu.cpuId() == execute.FIcore) && (!execute.faultIsInjected) && (execute.FIcomponent == 10) && (curTick() >= execute.FItick) && (execute.FIentry == inst->id.execSeqNum) && (execute.FIbit<256))
		{
			//note that tick is not ==.
			if (execute.FIbit>=32)
				execute.FIbit-=32;
			
			else
			{
				IntReg injectBit = pow (2, execute.FIbit);
				DPRINTF(faultInjectionTrack, "************Fault is activating**************\nCPU:%d:In function %s, instruction %s original data is %d\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), val);
				val = val xor injectBit;
				DPRINTF(faultInjectionTrack, "************Fault is activated**************\nCPU:%d:In function %s, instruction %s data is now %d\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), val);
				
				execute.faultIsInjected=true;
			}
			// addr will be done in readMem
			
		}
		
		
		//HWISOO. soft error in data or ldr (LSQ)
		/*
		if ( (cpu.cpuId() == execute.FIcore) && (!execute.faultIsInjected) && (execute.oldFIcomponent == 6) && (curTick() >= execute.FItick) && (execute.FIentry == inst->id.execSeqNum) && (execute.FIbit<128))
		{
			//note that tick is not ==.
			if (execute.FIbit>=32)
				execute.FIbit-=32;
			
			else
			{
				IntReg injectBit = pow (2, execute.FIbit);
				DPRINTF(faultInjectionTrack, "************Fault is activating**************\nCPU:%d:In function %s, instruction %s original data is %d\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), val);
				val = val xor injectBit;
				DPRINTF(faultInjectionTrack, "************Fault is activated**************\nCPU:%d:In function %s, instruction %s data is now %d\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), val);
			}
			// addr will be done in readMem
			execute.faultIsInjected=true;
		}
		*/
		
		//HWISOO. permanent to FU
		if ( (cpu.cpuId() == execute.FIcore)  && curTick() >= execute.FItick && execute.FIentry<2 
		&& ( ((execute.FIcomponent >=13 && execute.FIcomponent <=17)&&!si->isLoad()) || (execute.FIcomponent == 18 && si->isLoad()) ))
		{
			if( ( (funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") ) && cpu.getContext(inst->id.threadId)->insideNonRepeatable == false)
			{
				/*
				if ( ( ((int)(si->disassemble(0).find(" ld"))) >=0 ) or ( ((int)(si->disassemble(0).find(" vld"))) >=0 ) )
				{
					//HWISOO. fix it because it is not int ALU operation (this is load)
					//NOPE
				}
				else
				*/
			
				if((((inst->fuIndex) + 13)==execute.FIcomponent) || execute.FIcomponent==18)
				{
					
					if(setPreviousSeqNum!=inst->id.execSeqNum)
						setOperandCountInSameSeq=0;
					else
						setOperandCountInSameSeq++;
					setPreviousSeqNum = inst->id.execSeqNum;
					
					//if(execute.FIentry ==setOperandCountInSameSeq)
					if((execute.FIbit%64)/32 ==setOperandCountInSameSeq)
					{					
						IntReg injectBit;
						if (execute.FIbit<64) // makes 0 //note that it is not 32. the reason is to fit this for float/floatbits
						{
							injectBit = (pow (2, execute.FIbit%32)); // ex) 1111101111111...
							injectBit = ~injectBit;
							val = val & injectBit;
						}
						else if (execute.FIbit>=64 && execute.FIbit<128)// makes 1
						{
							injectBit = (pow (2, (execute.FIbit-64)%32)); // ex) 0000010000000...
							val = val | injectBit;
						}
					}
				}
			}
		}
		
		
		//HWISOO. temporal to FU
		if ( (cpu.cpuId() == execute.FIcore) && (!execute.faultIsInjected) && (curTick() >= execute.FItick) && (execute.FIentry == inst->id.execSeqNum) && ( ((execute.FIcomponent >=3 && execute.FIcomponent <=7)&&!si->isLoad()) || (execute.FIcomponent == 8 && si->isLoad()) ) && int(execute.FIbit / 32) == idx)
		{
			
			if ( ((funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") )
				&& (((inst->fuIndex) + 3)==execute.FIcomponent || (execute.FIcomponent==8)) )
			{
				DPRINTF(faultInjectionTrack, "************Fault is activating**************\nCPU:%d:In function %s, instruction %s original integer data of FU %d is %d\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), inst->fuIndex, val);
				IntReg injectBit;
				injectBit = (pow (2, execute.FIbit%32)); // ex) 0001000...
				val = val xor injectBit;
				DPRINTF(faultInjectionTrack, "************Fault is activated**************\nCPU:%d:In function %s, instruction %s data of FU %d is now %d\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), inst->fuIndex, val);
				
				execute.faultIsInjected=true;
			}
		}	
		
		//HWISOO. permanent injection (to LSQ)
		/*
		if ( (cpu.cpuId() == execute.FIcore) && (execute.oldFIcomponent == 15) && (execute.FIbit<256) && prevTargetLSQSeqNum == inst->id.execSeqNum)
		{
			if( ( (funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") ) && curTick() >= execute.FItick && cpu.getContext(inst->id.threadId)->insideNonRepeatable == false)
			{
				//HWISOO: we don't need it as we have prevTargetLSQSeqNum
				//if ( ( ((int)(si->disassemble(0).find(" ld"))) >=0 ) or ( ((int)(si->disassemble(0).find(" vld"))) >=0 ) )
				{
					if (execute.FIbit<128) //makes 0
					{
						unsigned int injection_bit=((execute.FIbit)%32);
						unsigned int injection_idx=((execute.FIbit)/32);
						if (injection_idx==idx)
						{
							IntReg injectBit = pow (2, injection_bit);
							val = val & (~injectBit);
						}
					}
					
					else //makes 1
					{
						unsigned int injection_bit=((execute.FIbit-128)%32);
						unsigned int injection_idx=((execute.FIbit-128)/32);
						if (injection_idx==idx)
						{
							IntReg injectBit = pow (2, injection_bit);
							val = val | injectBit;
						}
					}
				}
			}
		}
		*/
		
		//HWISOO. permanent to register pointer
		if ( (cpu.cpuId() == execute.FIcore) && (execute.FIcomponent == 19) && (execute.FIentry == 2))
		{
			if( ( (funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") ) && curTick() >= execute.FItick && cpu.getContext(inst->id.threadId)->insideNonRepeatable == false)
			{
					int corruptedIdx=si->destRegIdx(idx);
					if(execute.FIbit <4)
					{
						int temp = pow (2, (execute.FIbit));				
						corruptedIdx=corruptedIdx&(~temp);
					}
					else
					{
						int temp = pow (2, (execute.FIbit-4));				
						corruptedIdx=corruptedIdx|temp;				
					}
					
					thread.setIntReg(corruptedIdx, val);
					return;
			}
		}
		
        thread.setIntReg(si->destRegIdx(idx), val);
    }

    void
    setFloatRegOperand(const StaticInst *si, int idx,
        TheISA::FloatReg val)
    {
		std::string funcName="nothing";
		Addr sym_addr;
		debugSymbolTable->findNearestSymbol(cpu.getContext(0)->instAddr(), funcName, sym_addr);

        int reg_idx = si->destRegIdx(idx) - TheISA::FP_Reg_Base;
		
		if( (funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") )
		{
			DPRINTF(registerAccesses, "cpu%d/%s/%d/writeFloat/%d/%d/FU=%d/isLoad=%d\n", cpu.cpuId(), si->disassemble(0), inst->id.execSeqNum ,reg_idx, idx, inst->fuIndex, si->isLoad()); //HWISOO
			if (thread.readFloatReg(reg_idx)==val)
			{
				DPRINTF(silentnessProfile, "cpu%d/sile/float\n", cpu.cpuId()); //HWISOO
			}
			else
			{
				DPRINTF(silentnessProfile, "cpu%d/diff/float\n", cpu.cpuId()); //HWISOO
			}
		}
		//HWISOO. temporal to register pointer
		if ( (cpu.cpuId() == execute.FIcore) && (!execute.faultIsInjected) && (execute.FIcomponent == 9) && (curTick() == execute.FItick) && (execute.FIentry == inst->id.execSeqNum) && (int((execute.FIbit) / 8) == idx ))
		{
			int temp = pow (2, (execute.FIbit) % 8);
			int corruptedIdx = temp xor reg_idx;
			execute.faultIsInjected=true;
			
		    DPRINTF(faultInjectionTrack, "************Fault is activated**************\nCPU:%d:In function %s, instruction %s is reading float register (%d -> %d)\n", cpu.cpuId(), funcName, si->disassemble(0), reg_idx, corruptedIdx);
			
			thread.setFloatReg(corruptedIdx, val);
			return;
		}
		
		//HWISOO. permanent to register pointer
		if ( (cpu.cpuId() == execute.FIcore) && (execute.FIcomponent == 19) && (execute.FIentry == 2))
		{
			if( ( (funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") ) && curTick() >= execute.FItick && cpu.getContext(inst->id.threadId)->insideNonRepeatable == false)
			{
					int corruptedIdx=reg_idx;
					if(execute.FIbit <4)
					{
						int temp = pow (2, (execute.FIbit));				
						corruptedIdx=corruptedIdx&(~temp);
					}
					else
					{
						int temp = pow (2, (execute.FIbit-4));				
						corruptedIdx=corruptedIdx|temp;				
					}
					
					thread.setFloatReg(corruptedIdx, val);
					return;
			}
		}
		

		//FI to data of vldr instruction is below(not this. component == 15)
		//HWISOO. soft error in data or ldr (LSQ)
		/*
		if ( (cpu.cpuId() == execute.FIcore) && (!execute.faultIsInjected) && (execute.oldFIcomponent == 6) && (curTick() >= execute.FItick) && (execute.FIentry == inst->id.execSeqNum) && (execute.FIbit<128))
		{
			//note that tick is not ==.
			if (execute.FIbit>=32)
				execute.FIbit-=32;
			
			else
			{
				union fp_bit_twiddler {
					TheISA::FloatReg f;
					TheISA::FloatRegBits i;
				} injectBit, temp;
				injectBit.i = pow (2, execute.FIbit);
				temp.f = val;
				
				
				//TheISA::FloatReg injectBit = pow (2, execute.FIbit);
				DPRINTF(faultInjectionTrack, "************Fault is activating**************\nCPU:%d:In function %s, instruction %s original data is %d\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), val);
				
				temp.i = temp.i xor injectBit.i;
				val = temp.f;
				
				//val = val xor injectBit;
				DPRINTF(faultInjectionTrack, "************Fault is activated**************\nCPU:%d:In function %s, instruction %s data is now %d\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), val);
				// addr will be done in readMem
				execute.faultIsInjected=true;
			}

			
		}
		*/
		
		//HWISOO. permanent injection (to LSQ)
		/*
		if ( (cpu.cpuId() == execute.FIcore) && (execute.oldFIcomponent == 15) && (execute.FIbit<256) && prevTargetLSQSeqNum == inst->id.execSeqNum)
		{
			if( ( (funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") ) && curTick() >= execute.FItick && cpu.getContext(inst->id.threadId)->insideNonRepeatable == false)
			{
				//if ( ( ((int)(si->disassemble(0).find(" ld"))) >=0 ) or ( ((int)(si->disassemble(0).find(" vld"))) >=0 ) )
				{
					union fp_bit_twiddler {
						TheISA::FloatReg f;
						TheISA::FloatRegBits i;
					} injectBit, temp;
					if (execute.FIbit<128) //makes 0
					{
						unsigned int injection_bit=((execute.FIbit)%32);
						unsigned int injection_idx=((execute.FIbit)/32);
						if (injection_idx==idx)
						{
							injectBit.i = (pow (2, injection_bit)); //ex)111111011111...
							injectBit.i = ~injectBit.i;
							temp.f = val;
							temp.i = temp.i & injectBit.i;
							val = temp.f;
						}
					}
					
					else //makes 1
					{
						unsigned int injection_bit=((execute.FIbit-128)%32);
						unsigned int injection_idx=((execute.FIbit-128)/32);
						if (injection_idx==idx)
						{
							injectBit.i = (pow (2, injection_bit)); //ex) 0000010000000...
							temp.f = val;
							temp.i = temp.i | injectBit.i;
							val = temp.f;
						}
					}
				}
			}
		}
		*/
		
		//HWISOO. permanent to FU
		if ( (cpu.cpuId() == execute.FIcore)  && curTick() >= execute.FItick && execute.FIentry<2 
		&& ( ((execute.FIcomponent >=13 && execute.FIcomponent <=17)&&!si->isLoad()) || (execute.FIcomponent == 18 && si->isLoad()) ))
		{
			if( ( (funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") ) && cpu.getContext(inst->id.threadId)->insideNonRepeatable == false)
			{
				/*
				if ( ( ((int)(si->disassemble(0).find(" ld"))) >=0 ) or ( ((int)(si->disassemble(0).find(" vld"))) >=0 ) )
				{
					//old_HWISOO. fix it because it is not int ALU operation (this is load)
					//HWISOO. it also be executed at FU 5 for setFloatBits
				}
				
				else
				*/
				if((((inst->fuIndex) + 13)==execute.FIcomponent) || execute.FIcomponent==18)
				{
					
					if(setPreviousSeqNum!=inst->id.execSeqNum)
						setOperandCountInSameSeq=0;
					else
						setOperandCountInSameSeq++;
					setPreviousSeqNum = inst->id.execSeqNum;
					
					if(execute.FIentry ==setOperandCountInSameSeq)
					{
						union fp_bit_twiddler {
							TheISA::FloatReg f;
							TheISA::FloatRegBits i;
						} injectBit, temp;
						if (execute.FIbit>=0 && execute.FIbit<(64)) // makes 0
						{
							unsigned int injection_bit=((execute.FIbit)%32);
							unsigned int injection_idx=((execute.FIbit)/32);
							if (injection_idx==idx)
							{
								injectBit.i = (pow (2, injection_bit)); //ex)111111011111...
								injectBit.i = ~injectBit.i;
								temp.f = val;
								temp.i = temp.i & injectBit.i;
								val = temp.f;
								/*
								injectBit = (pow (2, execute.FIbit)); // ex) 1111101111111...
								injectBit = ~injectBit;
								val = val & injectBit;
								*/
							}


						}
						else if (execute.FIbit>=(64) && execute.FIbit<128)// makes 1
						{
							unsigned int injection_bit=((execute.FIbit-64)%32);
							unsigned int injection_idx=((execute.FIbit-64)/32);
							if (injection_idx==idx)
							{
								injectBit.i = (pow (2, injection_bit)); //ex) 0000010000000...
								temp.f = val;
								temp.i = temp.i | injectBit.i;
								val = temp.f;
								/*
								injectBit = (pow (2, execute.FIbit-32)); // ex) 0000010000000...
								val = val | injectBit;
								*/
							}
						}
					}
				}
			}
		}
		
		//HWISOO. temporal to FU
		if ( (cpu.cpuId() == execute.FIcore) && (!execute.faultIsInjected) && (curTick() == execute.FItick) && (execute.FIentry == inst->id.execSeqNum) && ( ((execute.FIcomponent >=3 && execute.FIcomponent <=7)&&!si->isLoad()) || (execute.FIcomponent == 8 && si->isLoad()) ) && int(execute.FIbit / 32) == idx)
		{
			if ( ((funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") )
				&& (((inst->fuIndex) + 3)==execute.FIcomponent || (execute.FIcomponent==8)) )
			{
				unsigned int injection_bit=(execute.FIbit%32);
				
				union fp_bit_twiddler {
					TheISA::FloatReg f;
					TheISA::FloatRegBits i;
				} injectBit, temp;
				injectBit.i = pow (2, injection_bit);
				temp.f = val;
				
				DPRINTF(faultInjectionTrack, "************Fault is activating**************\nCPU:%d:In function %s, instruction %s original float data of FU (%d) (idx %d reg_idx %d) is %f\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), inst->fuIndex, idx, reg_idx,  val);
				
				temp.i = temp.i xor injectBit.i;
				val = temp.f;
				
				DPRINTF(faultInjectionTrack, "************Fault is activated**************\nCPU:%d:In function %s, instruction %s data (idx %d reg_idx %d) is now %f\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), idx, reg_idx,  val);
				
				execute.faultIsInjected=true;
			}
		}
		
        thread.setFloatReg(reg_idx, val);
    }
	
    void
    setFloatRegOperandBits(const StaticInst *si, int idx,
        TheISA::FloatRegBits val)
    {
		std::string funcName="nothing";
		Addr sym_addr;
		debugSymbolTable->findNearestSymbol(cpu.getContext(0)->instAddr(), funcName, sym_addr);

		int reg_idx = si->destRegIdx(idx) - TheISA::FP_Reg_Base;
		
		if( (funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") )
		{
			DPRINTF(registerAccesses, "cpu%d/%s/%d/writeFloatBits/%d/%d/FU=%d/isLoad=%d\n", cpu.cpuId(), si->disassemble(0), inst->id.execSeqNum ,reg_idx, idx, inst->fuIndex, si->isLoad()); //HWISOO
			if (thread.readFloatRegBits(reg_idx)==val)
			{
				DPRINTF(silentnessProfile, "cpu%d/sile/floatbits\n", cpu.cpuId()); //HWISOO
			}
			else
			{
				DPRINTF(silentnessProfile, "cpu%d/diff/floatbits\n", cpu.cpuId()); //HWISOO
			}
		}
		//HWISOO temporal to register pointer
		if ( (cpu.cpuId() == execute.FIcore) && (!execute.faultIsInjected) && (execute.FIcomponent == 9) && (curTick() == execute.FItick) && (execute.FIentry == inst->id.execSeqNum) && (int((execute.FIbit) / 8) == idx ))
		{
			int temp = pow (2, (execute.FIbit) % 8);
			int corruptedIdx = temp xor reg_idx;
			execute.faultIsInjected=true;
			
		    DPRINTF(faultInjectionTrack, "************Fault is activated**************\nCPU:%d:In function %s, instruction %s is reading float register (%d -> %d)\n", cpu.cpuId(), funcName, si->disassemble(0), reg_idx, corruptedIdx);
			
			thread.setFloatRegBits(corruptedIdx, val);
			return;
		}
		
		
		//HWISOO. permanent to register pointer
		if ( (cpu.cpuId() == execute.FIcore) && (execute.FIcomponent == 19) && (execute.FIentry == 2))
		{
			if( ( (funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") ) && curTick() >= execute.FItick && cpu.getContext(inst->id.threadId)->insideNonRepeatable == false)
			{
					int corruptedIdx=reg_idx;
					if(execute.FIbit <4)
					{
						int temp = pow (2, (execute.FIbit));				
						corruptedIdx=corruptedIdx&(~temp);
					}
					else
					{
						int temp = pow (2, (execute.FIbit-4));				
						corruptedIdx=corruptedIdx|temp;				
					}
					
					thread.setFloatRegBits(corruptedIdx, val);
					return;
			}
		}
		
		//FI to data of vldr instruction is below (component == 15)
		//HWISOO. soft error in data or ldr (LSQ)
		/*
		if ( (cpu.cpuId() == execute.FIcore) && (!execute.faultIsInjected) && (execute.oldFIcomponent == 6) && (curTick() >= execute.FItick) && (execute.FIentry == inst->id.execSeqNum) && (execute.FIbit<128))
		{
			//note that tick is not ==.
			if (execute.FIbit>=32)
				execute.FIbit-=32;
			
			else
			{
				TheISA::FloatRegBits injectBit = pow (2, execute.FIbit);
				DPRINTF(faultInjectionTrack, "************Fault is activating**************\nCPU:%d:In function %s, instruction %s original data is %d\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), val);
				val = val xor injectBit;
				DPRINTF(faultInjectionTrack, "************Fault is activated**************\nCPU:%d:In function %s, instruction %s data is now %d\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), val);
				// addr will be done in readMem
				execute.faultIsInjected=true;
			}

			
		}
		*/
		
		//HWISOO. permanent injection (to LSQ)
		/*
		if ( (cpu.cpuId() == execute.FIcore) && (execute.oldFIcomponent == 15) && (execute.FIbit<256) && prevTargetLSQSeqNum == inst->id.execSeqNum)
		{
			if( ( (funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") ) && curTick() >= execute.FItick && cpu.getContext(inst->id.threadId)->insideNonRepeatable == false)
			{
				//if ( ( ((int)(si->disassemble(0).find(" ld"))) >=0 ) or ( ((int)(si->disassemble(0).find(" vld"))) >=0 ) )
				{
					if (execute.FIbit<128) //makes 0
					{
						unsigned int injection_bit=((execute.FIbit)%32);
						unsigned int injection_idx=((execute.FIbit)/32);
						if (injection_idx==idx)
						{
							TheISA::FloatRegBits injectBit =  (pow (2, injection_bit)); // ex) 1111101111111...
							injectBit = ~injectBit;
							val = val & injectBit;
						}
					}
					
					else //makes 1
					{
						unsigned int injection_bit=((execute.FIbit-128)%32);
						unsigned int injection_idx=((execute.FIbit-128)/32);
						if (injection_idx==idx)
						{
							TheISA::FloatRegBits injectBit =  (pow (2, injection_bit)); //ex) 0000010000000...
							val = val | injectBit;
						}
					}
				}
			}
		}
		*/
		
		//HWISOO. permanent to FU
		if ( (cpu.cpuId() == execute.FIcore)  && curTick() >= execute.FItick && execute.FIentry<2 
		&& ( ((execute.FIcomponent >=13 && execute.FIcomponent <=17)&&!si->isLoad()) || (execute.FIcomponent == 18 && si->isLoad()) ))
		{
			if( ( (funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") ) && cpu.getContext(inst->id.threadId)->insideNonRepeatable == false)
			{
				/*
				if ( ( ((int)(si->disassemble(0).find(" ld"))) >=0 ) or ( ((int)(si->disassemble(0).find(" vld"))) >=0 ) )
				{
					//HWISOO. fix it because it is not int ALU operation (this is load)
				}
				else
				*/
				if((((inst->fuIndex) + 13)==execute.FIcomponent) || execute.FIcomponent==18)
				{
					if(setPreviousSeqNum!=inst->id.execSeqNum)
						setOperandCountInSameSeq=0;
					else
						setOperandCountInSameSeq++;
					setPreviousSeqNum = inst->id.execSeqNum;
					
					if(execute.FIentry ==setOperandCountInSameSeq)
					{
						if (execute.FIbit>=0 && execute.FIbit<(64)) // makes 0
						{
							unsigned int injection_bit=((execute.FIbit)%32);
							unsigned int injection_idx=((execute.FIbit)/32);
							if (injection_idx==idx)
							{
								TheISA::FloatRegBits injectBit =  (pow (2, injection_bit)); // ex) 1111101111111...
								injectBit = ~injectBit;
								val = val & injectBit;
							}


						}
						else if (execute.FIbit>=(64) && execute.FIbit<128)// makes 1
						{
							unsigned int injection_bit=((execute.FIbit-64)%32);
							unsigned int injection_idx=((execute.FIbit-64)/32);
							if (injection_idx==idx)
							{
								TheISA::FloatRegBits injectBit =  (pow (2, injection_bit)); //ex) 0000010000000...
								val = val | injectBit;
							}
						}
					}
				}
			}
		}
		
		//HWISOO. temporal to FU
		if ( (cpu.cpuId() == execute.FIcore) && (!execute.faultIsInjected) && (curTick() >= execute.FItick) && (execute.FIentry == inst->id.execSeqNum) && ( ((execute.FIcomponent >=3 && execute.FIcomponent <=7)&&!si->isLoad()) || (execute.FIcomponent == 8 && si->isLoad()) ) && int(execute.FIbit / 32) == idx)
		{
			if ( ((funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") )
				&& (((inst->fuIndex) + 3)==execute.FIcomponent || (execute.FIcomponent==8)) )
			{
				unsigned int injection_bit=(execute.FIbit%32);

				
				DPRINTF(faultInjectionTrack, "************Fault is activating**************\nCPU:%d:In function %s, instruction %s original float bits data of FU %d (idx %d reg_idx %d) is %d\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), inst->fuIndex, idx, reg_idx, val);
				TheISA::FloatRegBits injectBit;
				injectBit = (pow (2, injection_bit)); // ex) 00001000000...
				val = val xor injectBit;
				DPRINTF(faultInjectionTrack, "************Fault is activated**************\nCPU:%d:In function %s, instruction %s float bits data of FU %d (idx %d reg_idx %d) is now %d\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), inst->fuIndex, idx, reg_idx, val);

				execute.faultIsInjected=true;
			}
		}
		
		//HWISOO. temporal to LSQ
		if ( (cpu.cpuId() == execute.FIcore) && (!execute.faultIsInjected) && (execute.FIcomponent == 10) && (curTick() >= execute.FItick) && (execute.FIentry == inst->id.execSeqNum) && (execute.FIbit<256))
		{
			//note that tick is not ==.
			if (execute.FIbit>=32)
				execute.FIbit-=32;
			
			else
			{
				TheISA::FloatRegBits injectBit = pow (2, execute.FIbit);
				DPRINTF(faultInjectionTrack, "************Fault is activating**************\nCPU:%d:In function %s, instruction %s original data is %d\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), val);
				val = val xor injectBit;
				DPRINTF(faultInjectionTrack, "************Fault is activated**************\nCPU:%d:In function %s, instruction %s data is now %d\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), val);
				
				
				execute.faultIsInjected=true;
			}
			// addr will be done in readMem
			
		}
		
		
		
        thread.setFloatRegBits(reg_idx, val);
    }

    bool
    readPredicate()
    {
        return thread.readPredicate();
    }

    void
    setPredicate(bool val)
    {
        thread.setPredicate(val);
    }

    TheISA::PCState
    pcState() const
    {
        return thread.pcState();
    }

    void
    pcState(const TheISA::PCState &val)
    {
        thread.pcState(val);
    }

    TheISA::MiscReg
    readMiscRegNoEffect(int misc_reg) const
    {
        return thread.readMiscRegNoEffect(misc_reg);
    }

    TheISA::MiscReg
    readMiscReg(int misc_reg)
    {
        return thread.readMiscReg(misc_reg);
    }

    void
    setMiscReg(int misc_reg, const TheISA::MiscReg &val)
    {
        thread.setMiscReg(misc_reg, val);
    }

    TheISA::MiscReg
    readMiscRegOperand(const StaticInst *si, int idx)
    {
        int reg_idx = si->srcRegIdx(idx) - TheISA::Misc_Reg_Base;
        return thread.readMiscReg(reg_idx);
    }

    void
    setMiscRegOperand(const StaticInst *si, int idx,
        const TheISA::MiscReg &val)
    {
        int reg_idx = si->destRegIdx(idx) - TheISA::Misc_Reg_Base;
        return thread.setMiscReg(reg_idx, val);
    }

    Fault
    hwrei()
    {
#if THE_ISA == ALPHA_ISA
        return thread.hwrei();
#else
        return NoFault;
#endif
    }

    bool
    simPalCheck(int palFunc)
    {
#if THE_ISA == ALPHA_ISA
        return thread.simPalCheck(palFunc);
#else
        return false;
#endif
    }

    void
    syscall(int64_t callnum)
    {
        if (FullSystem)
            panic("Syscall emulation isn't available in FS mode.\n");

        thread.syscall(callnum);
    }

    ThreadContext *tcBase() { return thread.getTC(); }

    /* @todo, should make stCondFailures persistent somewhere */
    unsigned int readStCondFailures() const { return 0; }
    void setStCondFailures(unsigned int st_cond_failures) {}

    ContextID contextId() { return thread.contextId(); }
    /* ISA-specific (or at least currently ISA singleton) functions */

    /* X86: TLB twiddling */
    void
    demapPage(Addr vaddr, uint64_t asn)
    {
        thread.getITBPtr()->demapPage(vaddr, asn);
        thread.getDTBPtr()->demapPage(vaddr, asn);
    }

    TheISA::CCReg
    readCCRegOperand(const StaticInst *si, int idx)
    {
        int reg_idx = si->srcRegIdx(idx) - TheISA::CC_Reg_Base;
		TheISA::CCReg originalValue = thread.readCCReg(reg_idx); //HWISOO

		
		std::string funcName="nothing";
		Addr sym_addr;
		debugSymbolTable->findNearestSymbol(cpu.getContext(0)->instAddr(), funcName, sym_addr);
		
		if( (funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") )
		{
			if (reg_idx != 5)
				DPRINTF(CCAccesses, "cpu%d/%s/%d/readCC/%d/%d/%d/%s/FU=%d\n", cpu.cpuId(), si->disassemble(0), inst->id.execSeqNum ,reg_idx, idx,originalValue, TheISA::ccRegName[reg_idx], inst->fuIndex); //HWISOO
		}
        return originalValue;
    }

    void
    setCCRegOperand(const StaticInst *si, int idx, TheISA::CCReg val)
    {
        int reg_idx = si->destRegIdx(idx) - TheISA::CC_Reg_Base;
	
		std::string funcName="nothing";
		Addr sym_addr;
		debugSymbolTable->findNearestSymbol(cpu.getContext(0)->instAddr(), funcName, sym_addr);
		
		if( (funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") )
			if (reg_idx != 5)
				DPRINTF(CCAccesses, "cpu%d/%s/%d/writeCC/%d/%d/%d/%s/FU=%d\n", cpu.cpuId(), si->disassemble(0), inst->id.execSeqNum ,reg_idx, idx, val, TheISA::ccRegName[reg_idx] , inst->fuIndex); //HWISOO
	
	
		//HWISOO. permanent error injection to cc ALU
		if ( (cpu.cpuId() == execute.FIcore) && ((execute.FIcomponent >=13 && execute.FIcomponent <=16)) && (execute.FIbit>=1024))
		{
			if( ( (funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") ) && curTick() >= execute.FItick && cpu.getContext(inst->id.threadId)->insideNonRepeatable == false)
			{
				/*
				if ( ( ((int)(si->disassemble(0).find(" ld"))) >=0 ) or ( ((int)(si->disassemble(0).find(" vld"))) >=0 ) )
				{
					//HWISOO. fix it because it is not int ALU operation (this is load)
				}
				else
				*/
				//for CCRegs, we should care if ldr operation changes CCregs.
				if( ((inst->fuIndex) + 13)==execute.FIcomponent )
				{
					if (execute.FIbit>=1024 && execute.FIbit<(1024+5)) // makes 0
					{
						unsigned int injection_idx=(execute.FIbit-1024);
						if (injection_idx==idx)
						{
							val = 0;
						}


					}
					else if (execute.FIbit>=(1024+5) && execute.FIbit<1024+5+5)// makes 1
					{
						unsigned int injection_idx=(execute.FIbit-1024-5);
						if (injection_idx==idx)
						{
							val = 1;
						}
					}
				}
			}
		}
		
		

	
		//HWISOO. soft error injection to cc ALU
		if ( (cpu.cpuId() == execute.FIcore) && (!execute.faultIsInjected) && (curTick() >= execute.FItick) && (execute.FIentry == inst->id.execSeqNum)&&execute.FIbit<64 && ((execute.FIcomponent >=3 && execute.FIcomponent <=6) && execute.FIbit>=1024) )
		{
			if (((funcName[0] == 'F' &&  funcName[1] == 'U' && funcName[2] == 'N' && funcName[3] == 'C') || (funcName == "main") ) && ((inst->fuIndex) + 3)==execute.FIcomponent )
			{
				unsigned int injection_idx=(execute.FIbit-1024);
				if (injection_idx == idx)
				{
					DPRINTF(faultInjectionTrack, "************Fault is activating**************\nCPU:%d:In function %s, instruction %s original ccreg( idx %d reg_idx %d ) data is %d\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), idx, reg_idx, val);
					TheISA::CCReg injectBit = 1;
					
					val = val xor injectBit;
					DPRINTF(faultInjectionTrack, "************Fault is activated**************\nCPU:%d:In function %s, instruction %s ccreg( idx %d reg_idx %d ) data is now %d\n", cpu.cpuId(), funcName, inst->staticInst->disassemble(0), idx, reg_idx, val);
					execute.faultIsInjected=true;
				}
			}
		}
	
        thread.setCCReg(reg_idx, val);
    }

    void
    demapInstPage(Addr vaddr, uint64_t asn)
    {
        thread.getITBPtr()->demapPage(vaddr, asn);
    }

    void
    demapDataPage(Addr vaddr, uint64_t asn)
    {
        thread.getDTBPtr()->demapPage(vaddr, asn);
    }

    /* ALPHA/POWER: Effective address storage */
    void setEA(Addr ea)
    {
        inst->ea = ea;
    }

    BaseCPU *getCpuPtr() { return &cpu; }

    /* POWER: Effective address storage */
    Addr getEA() const
    {
        return inst->ea;
    }

    /* MIPS: other thread register reading/writing */
    uint64_t
    readRegOtherThread(int idx, ThreadID tid = InvalidThreadID)
    {
        SimpleThread *other_thread = (tid == InvalidThreadID
            ? &thread : cpu.threads[tid]);

        if (idx < TheISA::FP_Reg_Base) { /* Integer */
            return other_thread->readIntReg(idx);
        } else if (idx < TheISA::Misc_Reg_Base) { /* Float */
            return other_thread->readFloatRegBits(idx
                - TheISA::FP_Reg_Base);
        } else { /* Misc */
            return other_thread->readMiscReg(idx
                - TheISA::Misc_Reg_Base);
        }
    }

    void
    setRegOtherThread(int idx, const TheISA::MiscReg &val,
        ThreadID tid = InvalidThreadID)
    {
        SimpleThread *other_thread = (tid == InvalidThreadID
            ? &thread : cpu.threads[tid]);

        if (idx < TheISA::FP_Reg_Base) { /* Integer */
            return other_thread->setIntReg(idx, val);
        } else if (idx < TheISA::Misc_Reg_Base) { /* Float */
            return other_thread->setFloatRegBits(idx
                - TheISA::FP_Reg_Base, val);
        } else { /* Misc */
            return other_thread->setMiscReg(idx
                - TheISA::Misc_Reg_Base, val);
        }
    }

  public:
    // monitor/mwait funtions
    void armMonitor(Addr address) { getCpuPtr()->armMonitor(0, address); }
    bool mwait(PacketPtr pkt) { return getCpuPtr()->mwait(0, pkt); }
    void mwaitAtomic(ThreadContext *tc)
    { return getCpuPtr()->mwaitAtomic(0, tc, thread.dtb); }
    AddressMonitor *getAddrMonitor()
    { return getCpuPtr()->getCpuAddrMonitor(0); }
};

}

#endif /* __CPU_MINOR_EXEC_CONTEXT_HH__ */
