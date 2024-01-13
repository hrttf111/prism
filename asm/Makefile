bios_test: bios_test.asm
	yasm -f bin -p nasm $< -l $@.list -o $@
	hexdump -C $@ > $@.map
	#cat $@ > $@.img
	#dd if=/dev/zero of=$@.img bs=1 count=1408511 seek=66049

test_exec: test_exec.asm
	yasm -f bin -p nasm $< -l $@.list -o $@
	hexdump -C $@ > $@.map

.PHONY: clean

clean:
	rm -f bios_test text_exec *.map *.list
