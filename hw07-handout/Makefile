# hw07
# Handout version 
# Writeup version 
# Library version 

HANDINURL := http://unofficial.fish.ics.cs.cmu.edu/officialSubmit.rb

.PHONY: all run clean submit
all: hw07-handin.tgz

# This Makefile gets handed in too as part of a just-in-case
# paper trail so that, if issues arise, we can tell what
# version of the handout you were working with. Be sure to
# stay on top of email and Piazza announcements so you'll
# know if we have to issue an emergency post-release fix!
hw07-handin.tgz: basix.sml cooking2.sml cooking.sml fs.sml hw07.pdf minix.sml regex.sml test-all.sml 
	tar -czf hw07-handin.tgz Makefile $^

# Do not call 'make run' as a student for a non-modular assignment
run: sources.cm
	mkdir make-run-backup
	echo " end" > suff.txt
	for file in `ls *.sml`; do cp $$file make-run-backup
		f=`echo $$file | sed 's/\(.*\)\..*/\1/'` && fstr=`echo $$f | sed 's/\(.\)\(.*\)/\U\1\E\2/'` && fsig=`echo $$f | sed 's/\(.*\)/\U\1/'` && echo "structure $$fstr : $$fsig = struct " > pref.txt && grep -q 'structure\|functor' $$file || (cat pref.txt $$file suff.txt >> cat.txt && mv cat.txt $$file); done
	rm -f pref.txt suff.txt
	sml -m sources.cm
	mv -f make-run-backup/* .
	rmdir make-run-backup

clean:
	rm -f hw07-handin.tgz
	rm -Rf .cm

# "I accidentally submitted old LaTeX..."  Don't be that guy.
%.pdf: %.tex
	@echo '$@ is older than $<'
	@echo -n 'Would you like to run pdflatex? [Y/n] '
	@read ANS; if [ -z "$$ANS" -o "$$ANS" = y -o "$$ANS" = Y ]; then echo pdflatex $<; pdflatex $<; fi

# Submit your solution to autolab
submit: hw07-handin.tgz
	hostname | grep 'qatar\.cmu\.edu\|andrew\.cmu\.edu\|cs\.cmu\.edu' || { echo "You're not on a CMU machine!"; exit 1; }
	cp hw07-handin.tgz `wget -q -O- "$(HANDINURL)?course=15150q-f14&user=$(USER)&assessment=hw07"`
	wget -q -O- "$(HANDINURL)?course=15150q-f14&user=$(USER)&assessment=hw07&submit=hw07-handin.tgz" > grep 'Submission received' || echo "Submission Uploaded, make sure you check your submission on the autolab webpage"
