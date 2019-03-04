/*
 * Copyright (c) 2013 ARM Limited
 * All rights reserved.
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
 * Copyright (c) 2009 The Regents of The University of Michigan
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
 * Authors: Lisa Hsu
 */

/**
 * @file
 * Declaration of an associative set
 */

#ifndef __MEM_CACHE_TAGS_CACHESET_HH__
#define __MEM_CACHE_TAGS_CACHESET_HH__

#include <cassert>
#include "debug/CacheHwisooDebug.hh"
#include "debug/faultInjectionTrack.hh"


/**
 * An associative set of cache blocks.
 */
template <class Blktype>
class CacheSet
{
  public:
    /** The associativity of this set. */
    int assoc;

    /** Cache blocks in this set, maintained in LRU order 0 = MRU. */
    Blktype **blks;

    /**
     * Find a block matching the tag in this set.
     * @param way_id The id of the way that matches the tag.
     * @param tag The Tag to find.
     * @param is_secure True if the target memory space is secure.
     * @return Pointer to the block if found. Set way_id to assoc if none found
     */
    Blktype* findBlk(Addr tag, bool is_secure, int& way_id) const ;
    Blktype* findBlk(Addr tag, bool is_secure) const ;

    /**
     * Move the given block to the head of the list.
     * @param blk The block to move.
     */
    void moveToHead(Blktype *blk);

    /**
     * Move the given block to the tail of the list.
     * @param blk The block to move
     */
    void moveToTail(Blktype *blk);

	//HWISOO
	void testPrint(Addr tag, bool is_secure);
		
	//HWISOO
	void injectTemporalFault(unsigned int FIbit);
	void injectPermanentFault(unsigned int FIbit);
};

template <class Blktype>
Blktype*
CacheSet<Blktype>::findBlk(Addr tag, bool is_secure, int& way_id) const
{
    /**
     * Way_id returns the id of the way that matches the block
     * If no block is found way_id is set to assoc.
     */
    way_id = assoc;
    for (int i = 0; i < assoc; ++i) {
        if (blks[i]->tag == tag && blks[i]->isValid() &&
            blks[i]->isSecure() == is_secure) {
            way_id = i;
            return blks[i];
        }
    }
    return NULL;
}

template <class Blktype>
Blktype*
CacheSet<Blktype>::findBlk(Addr tag, bool is_secure) const
{
    int ignored_way_id;
    return findBlk(tag, is_secure, ignored_way_id);
}

template <class Blktype>
void
CacheSet<Blktype>::moveToHead(Blktype *blk)
{
    // nothing to do if blk is already head
    if (blks[0] == blk)
        return;

    // write 'next' block into blks[i], moving up from MRU toward LRU
    // until we overwrite the block we moved to head.

    // start by setting up to write 'blk' into blks[0]
    int i = 0;
    Blktype *next = blk;

    do {
        assert(i < assoc);
        std::swap(blks[i], next);
        ++i;
    } while (next != blk);
}

template <class Blktype>
void
CacheSet<Blktype>::moveToTail(Blktype *blk)
{
    // nothing to do if blk is already tail
    if (blks[assoc - 1] == blk)
        return;

    // write 'next' block into blks[i], moving from LRU to MRU
    // until we overwrite the block we moved to tail.

    // start by setting up to write 'blk' into tail
    int i = assoc - 1;
    Blktype *next = blk;

    do {
        assert(i >= 0);
        std::swap(blks[i], next);
        --i;
    } while (next != blk);
}

template <class Blktype>
void
CacheSet<Blktype>::testPrint(Addr tag, bool is_secure)
{
	
	DPRINTF(CacheHwisooDebug, "assoc:%d\n", assoc);
    for (int i = 0; i < assoc; ++i) {
		DPRINTF(CacheHwisooDebug, "valid:%d/size:%d\n", blks[i]->isValid(), blks[i]->size);
        DPRINTF(CacheHwisooDebug, "data[0]:%d/data[127]:%d\n", blks[i]->data[0], blks[i]->data[127]);
    }
	
    /*HWISOO.
	1. size looks always 128.
	-> Then, can we "randomly" select this 128, or can select INDEX of this?(base_set_assoc does not calculate index)
	2. assoc looks different, but currently it is quite hard to distinguish they are same, or different cache
	   it means, how can we know that it is data/icache/l2cache and core number? (even in base_set_assoc)
	   
	
	
	*/
}

template <class Blktype>
void
CacheSet<Blktype>::injectTemporalFault(unsigned int FIbit)
{
	unsigned int way = FIbit/2048; 
	unsigned int injectionByte = (FIbit%2048)/8;
	uint8_t injectionBit = (FIbit)%8;
	
	uint8_t originalByte= blks[way]->data[injectionByte];
	uint8_t temp = pow (2, injectionBit);
	uint8_t faultyByte=originalByte xor temp;

	blks[way]->data[injectionByte] = faultyByte;
	
	DPRINTF(faultInjectionTrack, "FI temporal to cache(inside of cacheset). byte: [%d] is fliiped (%d -> %d), %s\n", injectionByte, originalByte, faultyByte, (blks[way]->isValid())?"injected(valid)":"will be masked(invalid");
}



template <class Blktype>
void
CacheSet<Blktype>::injectPermanentFault(unsigned int FIbit)
{
	//unsigned int way = FIbit/2048; 
	//unsigned int injectionByte = (FIbit%2048)/8;
	//uint8_t injectionBit = (FIbit)%8;
	unsigned int way = FIbit/2048;
	
	if((FIbit%2048)<1024) //make 0
	{
		unsigned int injectionByte=(FIbit%2048)/8;
		uint8_t injectionBit = (FIbit)%8;
		
		uint8_t originalByte=blks[way]->data[injectionByte];
		uint8_t temp = pow (2, injectionBit);
		uint8_t faultyByte=originalByte & (~temp);

		blks[way]->data[injectionByte] = faultyByte;
	}
	else //make 1
	{
		unsigned int injectionByte=(FIbit%2048 - 1024)/8;
		uint8_t injectionBit = (FIbit)%8;
		
		uint8_t originalByte=blks[way]->data[injectionByte];
		uint8_t temp = pow (2, injectionBit);
		uint8_t faultyByte=originalByte | (temp);
										
		blks[way]->data[injectionByte] = faultyByte;
	}

	
	
}


#endif
