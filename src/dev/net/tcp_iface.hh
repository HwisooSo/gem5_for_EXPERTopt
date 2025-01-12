/*
 * Copyright (c) 2015 ARM Limited
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
 * Authors: Gabor Dozsa
 */

/* @file
 * TCP stream socket based interface class for multi gem5 runs.
 *
 * For a high level description about multi gem5 see comments in
 * header file multi_iface.hh.
 *
 * The TCP subclass of MultiIface uses a separate server process
 * (see tcp_server.[hh,cc] under directory gem5/util/multi). Each gem5
 * process connects to the server via a stream socket. The server process
 * transfers messages and co-ordinates the synchronisation among the gem5
 * peers.
 */
#ifndef __DEV_NET_TCP_IFACE_HH__
#define __DEV_NET_TCP_IFACE_HH__


#include <string>

#include "dev/net/multi_iface.hh"

class EventManager;

class TCPIface : public MultiIface
{
  private:
    /**
     * The stream socket to connect to the server.
     */
    int sock;

    /**
     * Registry for all sockets to the server opened by this gem5 process.
     */
    static std::vector<int> sockRegistry;

  private:

    /**
     * Send out a message through a TCP stream socket.
     *
     * @param sock TCP stream socket.
     * @param buf Start address of the message.
     * @param length Size of the message in bytes.
     */
    void
    sendTCP(int sock, void *buf, unsigned length);

    /**
     * Receive the next incoming message through a TCP stream socket.
     *
     * @param sock TCP stream socket.
     * @param buf Start address of buffer to store the message.
     * @param length Exact size of the expected message in bytes.
     */
    bool recvTCP(int sock, void *buf, unsigned length);


  protected:

    virtual void
    sendRaw(void *buf, unsigned length,
            const MultiHeaderPkt::AddressType dest_addr=nullptr) override
    {
        sendTCP(sock, buf, length);
    }

    virtual bool recvRaw(void *buf, unsigned length) override
    {
        return recvTCP(sock, buf, length);
    }

    virtual void syncRaw(MultiHeaderPkt::MsgType sync_req,
                         Tick sync_tick) override;

  public:
    /**
     * The ctor creates and connects the stream socket to the server.
     * @param server_name The name (or IP address) of the host running the
     * server process.
     * @param server_port The port number the server listening for new
     * connections.
     * @param sync_start The tick for the first multi synchronisation.
     * @param sync_repeat The frequency of multi synchronisation.
     * @param em The EventManager object associated with the simulated
     * Ethernet link.
     */
    TCPIface(std::string server_name, unsigned server_port,
             unsigned multi_rank, Tick sync_start, Tick sync_repeat,
             EventManager *em);

    ~TCPIface() override;
};

#endif // __DEV_NET_TCP_IFACE_HH__
