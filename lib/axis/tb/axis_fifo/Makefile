# Copyright (c) 2021 Alex Forencich
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

TOPLEVEL_LANG = vhdl

SIM ?= ghdl
WAVES ?= 0

COCOTB_HDL_TIMEUNIT = 1ns
COCOTB_HDL_TIMEPRECISION = 1ps

DUT      = axis_fifo
TOPLEVEL = $(DUT)
MODULE   = test_$(DUT)
#VERILOG_SOURCES += ../../rtl/$(DUT).v
VHDL_SOURCES += ../../../../vhdl/vhdl_pkg.vhd
VHDL_SOURCES += ../../../../vhdl/$(DUT).vhd

GHDL_ARGS += --std=08  -g -v 
PLUSARGS += --max-stack-alloc=262144 --backtrace-severity=note --wave=tb.ghw

# module parameters
export PARAM_DATA_WIDTH := 8
export PARAM_KEEP_ENABLE := $(shell echo $$(( $(PARAM_DATA_WIDTH) > 8 )))
export PARAM_KEEP_WIDTH := $(shell echo $$(( ( $(PARAM_DATA_WIDTH) + 7 ) / 8 )))
export PARAM_DEPTH := $(shell echo $$(( 1024 * $(PARAM_KEEP_WIDTH) )))
export PARAM_LAST_ENABLE := 1
export PARAM_ID_ENABLE := 1
export PARAM_ID_WIDTH := 8
export PARAM_DEST_ENABLE := 1
export PARAM_DEST_WIDTH := 8
export PARAM_USER_ENABLE := 1
export PARAM_USER_WIDTH := 1
export PARAM_RAM_PIPELINE := 1
export PARAM_OUTPUT_FIFO_ENABLE := 0
export PARAM_FRAME_FIFO := 1
export PARAM_USER_BAD_FRAME_VALUE := 1
export PARAM_USER_BAD_FRAME_MASK := 1
export PARAM_DROP_OVERSIZE_FRAME := $(PARAM_FRAME_FIFO)
export PARAM_DROP_BAD_FRAME := $(PARAM_DROP_OVERSIZE_FRAME)
export PARAM_DROP_WHEN_FULL := 0
export PARAM_MARK_WHEN_FULL := 0
export PARAM_PAUSE_ENABLE := 1
export PARAM_FRAME_PAUSE := 1

SIM_ARGS += -gDATA_WIDTH=$(PARAM_DATA_WIDTH)
SIM_ARGS += -gKEEP_ENABLE=$(PARAM_KEEP_ENABLE)
SIM_ARGS += -gKEEP_WIDTH=$(PARAM_KEEP_WIDTH)
SIM_ARGS += -gDEPTH=$(PARAM_DEPTH)
SIM_ARGS += -gLAST_ENABLE=$(PARAM_LAST_ENABLE)
SIM_ARGS += -gID_ENABLE=$(PARAM_ID_ENABLE)
SIM_ARGS += -gID_WIDTH=$(PARAM_ID_WIDTH)
SIM_ARGS += -gDEST_ENABLE=$(PARAM_DEST_ENABLE)
SIM_ARGS += -gDEST_WIDTH=$(PARAM_DEST_WIDTH)
SIM_ARGS += -gUSER_ENABLE=$(PARAM_USER_ENABLE)
SIM_ARGS += -gUSER_WIDTH=$(PARAM_USER_WIDTH)
SIM_ARGS += -gRAM_PIPELINE=$(PARAM_RAM_PIPELINE)
SIM_ARGS += -gOUTPUT_FIFO_ENABLE=$(PARAM_OUTPUT_FIFO_ENABLE)
SIM_ARGS += -gFRAME_FIFO=$(PARAM_FRAME_FIFO)
SIM_ARGS += -gUSER_BAD_FRAME_VALUE=$(PARAM_USER_BAD_FRAME_VALUE)
SIM_ARGS += -gUSER_BAD_FRAME_MASK=$(PARAM_USER_BAD_FRAME_MASK)
SIM_ARGS += -gDROP_OVERSIZE_FRAME=$(PARAM_DROP_OVERSIZE_FRAME)
SIM_ARGS += -gDROP_BAD_FRAME=$(PARAM_DROP_BAD_FRAME)
SIM_ARGS += -gDROP_WHEN_FULL=$(PARAM_DROP_WHEN_FULL)
SIM_ARGS += -gMARK_WHEN_FULL=$(PARAM_MARK_WHEN_FULL)
SIM_ARGS += -gPAUSE_ENABLE=$(PARAM_PAUSE_ENABLE)
SIM_ARGS += -gFRAME_PAUSE=$(PARAM_FRAME_PAUSE)

ifeq ($(SIM), icarus)
	PLUSARGS += -fst

	COMPILE_ARGS += $(foreach v,$(filter PARAM_%,$(.VARIABLES)),-P $(TOPLEVEL).$(subst PARAM_,,$(v))=$($(v)))

	ifeq ($(WAVES), 1)
		VERILOG_SOURCES += iverilog_dump.v
		COMPILE_ARGS += -s iverilog_dump
	endif
else ifeq ($(SIM), verilator)
	COMPILE_ARGS += -Wno-SELRANGE -Wno-WIDTH

	COMPILE_ARGS += $(foreach v,$(filter PARAM_%,$(.VARIABLES)),-G$(subst PARAM_,,$(v))=$($(v)))

	ifeq ($(WAVES), 1)
		COMPILE_ARGS += --trace-fst
	endif
endif

include $(shell cocotb-config --makefiles)/Makefile.sim

iverilog_dump.v:
	echo 'module iverilog_dump();' > $@
	echo 'initial begin' >> $@
	echo '    $$dumpfile("$(TOPLEVEL).fst");' >> $@
	echo '    $$dumpvars(0, $(TOPLEVEL));' >> $@
	echo 'end' >> $@
	echo 'endmodule' >> $@

clean::
	@rm -rf iverilog_dump.v
	@rm -rf dump.fst $(TOPLEVEL).fst
