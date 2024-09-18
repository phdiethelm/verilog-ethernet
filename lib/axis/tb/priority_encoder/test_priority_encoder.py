#!/usr/bin/env python
"""

Copyright (c) 2014-2018 Alex Forencich

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
from cocotb.triggers import Timer
from cocotb.regression import TestFactory


class TB(object):
    def __init__(self, dut):
        self.dut = dut

        self.log = logging.getLogger("cocotb.tb")
        self.log.setLevel(logging.DEBUG)


async def run_test1(dut):

    tb = TB(dut)

    print("test 1: one bit")

    for i in range(32):
        dut.input_unencoded.value = 1 << i

        await Timer(10, units='ns')

        assert dut.output_encoded == i
        assert dut.output_unencoded == 1 << i

    await Timer(100, units='ns')

async def run_test2(dut):
    tb = TB(dut)

    print("test 2: two bits")


    for i in range(32):
        for j in range(32):

            dut.input_unencoded.value = (1 << i) | (1 << j)

            await Timer(10, units='ns')

            assert dut.output_encoded == max(i,j)
            assert dut.output_unencoded == 1 << max(i,j)

    await Timer(100, units='ns')


if cocotb.SIM_NAME:

    factory = TestFactory(run_test1)
    factory.generate_tests()

    factory = TestFactory(run_test2)
    factory.generate_tests()

    # for test in [
    #             run_test_tuser_assert,
    #             run_test_init_sink_pause,
    #             run_test_init_sink_pause_reset,
    #             run_test_pause,
    #             run_test_overflow,
    #             run_test_oversize
    #         ]:
    #
    #    factory = TestFactory(test)
    #    factory.generate_tests()


# cocotb-test

tests_dir = os.path.dirname(__file__)
rtl_dir = os.path.abspath(os.path.join(tests_dir, '..', '..', 'rtl'))


@pytest.mark.parametrize("width", [32])
@pytest.mark.parametrize("lsb_high_priority", [1, 0])
def test_priority_encoder(request, width, lsb_high_priority):

    dut = "priority_encoder"
    module = os.path.splitext(os.path.basename(__file__))[0]
    toplevel = dut

    verilog_sources = [
        os.path.join(rtl_dir, f"{dut}.v"),
    ]

    parameters = {}
    WIDTH = 32
    LSB_HIGH_PRIORITY = 0
    parameters['WIDTH'] = width
    parameters['LSB_HIGH_PRIORITY'] = lsb_high_priority

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
