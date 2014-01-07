# Shedule Server Makefile

EC=erlc
NAME=SheduleServer
INCLUDES=-I ./include
OBJDIR=./ebin
OUTPUT=-o ./ebin
FILES=./src/*.erl
CLEAN=./ebin/*.beam
RELEASE=\"shedule-release-1\"
RELEASE_BUILD="systools:make_script($(RELEASE),[local]),halt()."

all: binaries
	@echo Making release...
	@erl -pa ./ebin -eval $(RELEASE_BUILD)

binaries:
	@echo Building $(NAME)...
	mkdir -p $(OBJDIR)
	@$(EC) $(INCLUDES) $(OUTPUT) $(FILES) && echo Done
	
clean:
	@rm $(CLEAN) && echo Done
	
clean_release:
	@rm *.script *.boot && echo Done