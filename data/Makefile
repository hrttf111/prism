bios_test: bios_test.asm
	yasm -f bin -p nasm $< -l $@.list -o $@
	hexdump -C $@ > bios_test.map

.PHONY: clean

clean:
	rm -f bios_test *.map *.list
