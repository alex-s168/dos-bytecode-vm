rm disk.img

nasm -f bin -O0 main.asm -o main.com
nasm -f bin -O0 test.asm -o test.bin

mkfs.msdos -C disk.img 1440 1>/dev/null

mcopy -i disk.img main.com ::/
mcopy -i disk.img test.bin ::/
if [ -d debug ]; then
  mcopy -s -i disk.img debug ::/
fi
mdir -i disk.img

