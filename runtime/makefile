# This is intended to be a template for makefile (default to C project)
# Change any directory/suffices/flags/compiler as needed.
# Auto-dependencies still cannot handle file-renaming issue

CC = gcc
CFLAGS = -g -Wall -I ./$(HDRDIR) 

SRCDIR = src
TESTDIR = test
OBJDIR= build
HDRDIR  = include

STATIC_LIB := $(OBJDIR)/libsoml.a
SRCS := $(wildcard $(SRCDIR)/*.c)
OBJS := $(SRCS:$(SRCDIR)/%.c=$(OBJDIR)/%.o)
DEPS := $(SRCS:$(SRCDIR)/%.c=$(OBJDIR)/%.d)

#vpath %.c $(SRCDIR) 

all : $(STATIC_LIB)

$(STATIC_LIB) : $(OBJS)
	ar -rc $@ $^

$(OBJS) : $(OBJDIR)/%.o : $(SRCDIR)/%.c
	$(CC) $(CFLAGS) -c $< -o $@

$(DEPS) : $(OBJDIR)/%.d : $(SRCDIR)/%.c
	$(CC) $(CFLAGS) $< -MM -MT $(patsubst %.d, %.o, $@) -MF $@

# gcc ... src/foo.c -MM -MT build/foo.o -MF build/foo.d
# -MM Used to generate dependency file 
#  (for some reason the .c files need to be included)
# -MF specify the target file name, and the output is written to -MF file
#  e.g. (build/foo.o : src/foo.c include/foo.h include/bar.h)

-include $(DEPS)

.PHONY : clean

clean :
	rm $(OBJS) $(DEPS) $(STATIC_LIB) 
