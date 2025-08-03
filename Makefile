# Makefile

AS=$(HOME)/work/zasm/zasm
AS_FLAGS = --z180 --target=ram -uw
AS_MI_SRCS = basic.asm
AS_MD_SRCS = lunaxp.asm
AS_FULL_SRC = xp-basic.asm

all: $(AS_FULL_SRC)
	$(AS) $(AS_FLAGS) $(AS_FULL_SRC)

$(AS_FULL_SRC): $(AS_MD_SRCS) $(AS_MI_SRCS)
	cat $(AS_MD_SRCS) $(AS_MI_SRCS) > $(AS_FULL_SRC)
