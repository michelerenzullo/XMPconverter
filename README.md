XMP Converter for Visual Studio, GCC, and Emscripten

There are some little differences:
1) Visual Studio project uses optparse(https://github.com/skeeto/optparse) has alternative to getopt and as default I've enabled parallel batch convert(using std::for_each that calls Intel oneTBB static lib)  
2) GCC Version  
    a) on Linux uses parallel batch with openMP(shared lib, you'll see #pragma omp parallel for)  
    b) on Windows parallel libraries with MinGW compiler are slows(both oneTBB and openMP) so I've disabled.  
3) Emscripten 
 - it doesn't use int main() so I've removed getopt/optparse 
 - parallel mode isn't supported by WASM
 - I've added minizip-ng(https://github.com/zlib-ng/minizip-ng), automatic zip all converted files, you will find an already compiled static lib inside emsdk folder, just copy and paste (when building from source I've used only the following options on CMAKE in order to reduce weight of lib and complexity: MZ_ZLIB=ON,MZ_COMPRESS_ONLY=ON,MZ_PKCRYPT=ON,C_FLAGS="-Os -m64").

For emscripten version, I've exported 3 functions: options, encode and decode

options( {input,output,title,size,min,max,group,primaries,size,strength,gamut,} )  
You'll find the usage of arguments inside test/xmp_webconverter.hml, function execute()

Note, both input and output arguments accept folders or files(separated with a pipe "|")
example: input:"convertmefolder/" output:"convertedfolder/" or input:"LUT1.cube|LUT2.cube|LUT3.cube..." output:"LUT1.xmp|LUT2.xmp|LUT3.xmp..."

All arguments are optionals, if you don't pass something, default values will be parsed(before compile remember to replace original embind.js with my patched version inside emsdk folder, source https://github.com/emscripten-core/emscripten/issues/6226#issuecomment-364771548).
You can:  
 1) set options providing input/output paths --> automatic conversion and zip outputs if multiple.  
 2) or if you desire, set options WITHOUT providing input/output paths and manually encode(input cube path,output xmp path)  
 3) manually decode(input xmp path, outuput cube path)
 