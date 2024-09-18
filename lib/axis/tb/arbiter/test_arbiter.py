#!/usr/bin/env python
"""

Copyright (c) 2021 Alex Forencich

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

"""

import itertools
import logging
import os
import random

import cocotb_test.simulator
import pytest

import cocotb
from cocotb.clock import Clock
from cocotb.triggers import RisingEdge, Timer
from cocotb.regression import TestFactory


class TB(object):
    def __init__(self, dut):
        self.dut = dut

        self.log = logging.getLogger("cocotb.tb")
        self.log.setLevel(logging.DEBUG)

        cocotb.start_soon(Clock(dut.clk, 10, units="ns").start())

    async def reset(self):
        self.dut.rst.setimmediatevalue(0)
        self.dut.acknowledge.setimmediatevalue(0)
        await RisingEdge(self.dut.clk)
        await RisingEdge(self.dut.clk)
        self.dut.rst.value = 1
        await RisingEdge(self.dut.clk)
        await RisingEdge(self.dut.clk)
        self.dut.rst.value = 0
        await RisingEdge(self.dut.clk)
        await RisingEdge(self.dut.clk)


async def run_test1(dut):

    tb = TB(dut)

    await tb.reset()
    print("test 1: one bit")

    for i in range(32):
        l = [i]
        k = 0
        for y in l:
            k = k | 1 << y

        dut.request.value = k
        await RisingEdge(dut.clk)

        dut.request.value = 0
        await RisingEdge(dut.clk)

        assert dut.grant == 1 << i
        assert dut.grant_encoded == i

        await RisingEdge(dut.clk)

    await Timer(100, units='ns')

    await RisingEdge(dut.clk)
    await RisingEdge(dut.clk)


async def run_test2(dut):

    tb = TB(dut)

    await tb.reset()
    print("test 2: two bits")

    for i in range(32):
        for j in range(32):
            l = [i, j]
            k = 0
            for y in l:
                k = k | 1 << y

            dut.request.value = k
            await RisingEdge(dut.clk)

            dut.request.value = 0
            await RisingEdge(dut.clk)

            assert dut.grant == 1 << max(l)
            assert dut.grant_encoded == max(l)

            dut.request.value = 0
            await RisingEdge(dut.clk)

    await Timer(100, units='ns')

    await RisingEdge(dut.clk)
    await RisingEdge(dut.clk)


async def run_test3(dut):

    tb = TB(dut)

    await tb.reset()
    print("test 3: five bits")

    for i in range(32):
        l = [(i*x) % 32 for x in [1,2,3,4,5]]
        k = 0
        for y in l:
            k = k | 1 << y

        dut.request.value = k
        await RisingEdge(dut.clk)

        dut.request.value = 0
        await RisingEdge(dut.clk)

        assert dut.grant == 1 << max(l)
        assert dut.grant_encoded == max(l)

        prev = int(dut.grant_encoded)

        await RisingEdge(dut.clk)

    await Timer(100, units='ns')

    await RisingEdge(dut.clk)
    await RisingEdge(dut.clk)


if cocotb.SIM_NAME:
    factory = TestFactory(run_test1)
    factory.generate_tests()

    factory = TestFactory(run_test2)
    factory.generate_tests()

    factory = TestFactory(run_test3)
    factory.generate_tests()

# cocotb-test

tests_dir = os.path.dirname(__file__)
rtl_dir = os.path.abspath(os.path.join(tests_dir, '..', '..', 'rtl'))



def test_arbiter(request):

    dut = "axis_fifo"
    module = os.path.splitext(os.path.basename(__file__))[0]
    toplevel = dut

    verilog_sources = [
        os.path.join(rtl_dir, f"{dut}.v"),
    ]

    parameters = {}

    parameters['PORTS'] =  32
    parameters['ARB_TYPE_ROUND_ROBIN'] = 0
    parameters['ARB_BLOCK'] = 1
    parameters['ARB_BLOCK_ACK'] = 0
    parameters['ARB_LSB_HIGH_PRIORITY'] = 0

    extra_env = {f'PARAM_{k}': str(v) for k, v in parameters.items()}

    sim_build = os.path.join(tests_dir, "sim_build",
        request.node.name.replace('[', '-').replace(']', ''))

    cocotb_test.simulator.run(
        python_search=[tests_dir],
        verilog_sources=verilog_sources,
        toplevel=toplevel,
        module=module,
        parameters=parameters,
        sim_build=sim_build,
        extra_env=extra_env,
    )
