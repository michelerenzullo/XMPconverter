#include "optparse.h"
#include "lightMD5.h"
#include <chrono>
#include <zlib.h>
#include <string>
#include <execution>
#include <ctime>
#include <sys/stat.h>
#include <dirent.h>
#include <direct.h>
#include <vector>

typedef std::string string;
typedef int32_t int32;
typedef uint8_t uint8;
typedef uint16_t uint16;
typedef uint32_t uint32;

int32 int_round(double n) { return n >= 0 ? n + .5 : n - .5; }

void shrink(double* to_shrink, uint16* shrinked, int size, int new_size) {
	//tetrahedral interpolation, rounded for uint16
	double ratio = (size - 1.0) / (new_size - 1.0);

	for (int32 i = 0, idx; i < new_size; ++i)
		for (int32 j = 0; j < new_size; ++j)
			for (int32 k = 0; k < new_size; ++k) {
				int lr = std::clamp((int)(i * ratio), 0, size - 1);
				int lg = std::clamp((int)(j * ratio), 0, size - 1);
				int lb = std::clamp((int)(k * ratio), 0, size - 1);
				int ur = std::clamp(lr + 1, 0, size - 1);
				int ug = std::clamp(lg + 1, 0, size - 1);
				int ub = std::clamp(lb + 1, 0, size - 1);
				double fR = (double)i * ratio - lr;
				double fG = (double)j * ratio - lg;
				double fB = (double)k * ratio - lb;

				idx = (i * new_size * new_size + j * new_size + k) * 3;
				if (fG >= fB && fB >= fR)
					for (uint32 l = 0; l < 3; ++l)
						shrinked[idx + l] = 0.5
						+ (float)((1 - fG) * (int_round((float)to_shrink[(lr * size * size + lg * size + lb) * 3 + l] * 65535))
							+ (fG - fB) * (int_round((float)to_shrink[(lr * size * size + ug * size + lb) * 3 + l] * 65535))
							+ (fB - fR) * (int_round((float)to_shrink[(lr * size * size + ug * size + ub) * 3 + l] * 65535))
							+ fR * (int_round((float)to_shrink[(ur * size * size + ug * size + ub) * 3 + l] * 65535)));

				else if (fB > fR && fR > fG)
					for (uint32 l = 0; l < 3; ++l)
						shrinked[idx + l] = 0.5
						+ (float)((1 - fB) * (int_round((float)to_shrink[(lr * size * size + lg * size + lb) * 3 + l] * 65535))
							+ (fB - fR) * (int_round((float)to_shrink[(lr * size * size + lg * size + ub) * 3 + l] * 65535))
							+ (fR - fG) * (int_round((float)to_shrink[(ur * size * size + lg * size + ub) * 3 + l] * 65535))
							+ fG * (int_round((float)to_shrink[(ur * size * size + ug * size + ub) * 3 + l] * 65535)));

				else if (fB > fG && fG >= fR)
					for (uint32 l = 0; l < 3; ++l)
						shrinked[idx + l] = 0.5
						+ (float)((1 - fB) * (int_round((float)to_shrink[(lr * size * size + lg * size + lb) * 3 + l] * 65535))
							+ (fB - fG) * (int_round((float)to_shrink[(lr * size * size + lg * size + ub) * 3 + l] * 65535))
							+ (fG - fR) * (int_round((float)to_shrink[(lr * size * size + ug * size + ub) * 3 + l] * 65535))
							+ fR * (int_round((float)to_shrink[(ur * size * size + ug * size + ub) * 3 + l] * 65535)));

				else if (fR >= fG && fG > fB)
					for (uint32 l = 0; l < 3; ++l)
						shrinked[idx + l] = 0.5
						+ (float)((1 - fR) * (int_round((float)to_shrink[(lr * size * size + lg * size + lb) * 3 + l] * 65535))
							+ (fR - fG) * (int_round((float)to_shrink[(ur * size * size + lg * size + lb) * 3 + l] * 65535))
							+ (fG - fB) * (int_round((float)to_shrink[(ur * size * size + ug * size + lb) * 3 + l] * 65535))
							+ fB * (int_round((float)to_shrink[(ur * size * size + ug * size + ub) * 3 + l] * 65535)));

				else if (fG > fR && fR >= fB)
					for (uint32 l = 0; l < 3; ++l)
						shrinked[idx + l] = 0.5
						+ (float)((1 - fG) * (int_round((float)to_shrink[(lr * size * size + lg * size + lb) * 3 + l] * 65535))
							+ (fG - fR) * (int_round((float)to_shrink[(lr * size * size + ug * size + lb) * 3 + l] * 65535))
							+ (fR - fB) * (int_round((float)to_shrink[(ur * size * size + ug * size + lb) * 3 + l] * 65535))
							+ fB * (int_round((float)to_shrink[(ur * size * size + ug * size + ub) * 3 + l] * 65535)));

				else if (fR >= fB && fB >= fG)
					for (uint32 l = 0; l < 3; ++l)
						shrinked[idx + l] = 0.5
						+ (float)((1 - fR) * (int_round((float)to_shrink[(lr * size * size + lg * size + lb) * 3 + l] * 65535))
							+ (fR - fB) * (int_round((float)to_shrink[(ur * size * size + lg * size + lb) * 3 + l] * 65535))
							+ (fB - fG) * (int_round((float)to_shrink[(ur * size * size + lg * size + ub) * 3 + l] * 65535))
							+ fG * (int_round((float)to_shrink[(ur * size * size + ug * size + ub) * 3 + l] * 65535)));

			}
}

bool dirExists(const string& path) {
	struct stat sb;
	if (stat(path.c_str(), &sb) == 0 && S_ISDIR(sb.st_mode))
		return 1;
	else
		return 0;
}

bool fileExists(const string& filename) {
	struct stat sb;
	if (stat(filename.c_str(), &sb) == 0 && S_ISREG(sb.st_mode))
		return 1;
	else
		return 0;
}

bool mkpath(string path) {
	string dir;
	for (size_t pos = 0; pos != string::npos; pos = path.find_first_of("/\\", pos)) {
		dir = path.substr(0, pos++);
		if (_mkdir(dir.c_str()) == -1 && fileExists(dir))
			return 0;
	}
	return 1;
}

void read_directory(const string& name, std::vector<string>& v) {
	DIR* dirp = opendir(name.c_str());
	struct dirent* dp;
	while ((dp = readdir(dirp)) != NULL) {
		char* check_extension;
		if (check_extension = strrchr(dp->d_name, '.')) {
			for (uint32 i = 0; i < strlen(check_extension); ++i)
				check_extension[i] = tolower(check_extension[i]);
			if (check_extension != NULL && (strcmp(check_extension, ".cube") == 0 xor strcmp(check_extension, ".xmp") == 0))
				v.push_back(name + "/" + string(dp->d_name));
		}
	}
	closedir(dirp);
}


static struct defaults {
	uint32 gamut = 0;
	uint32 size = 32;
	string outFileName;
	uint32 min = 0;
	uint32 max = 200;
	string amount;
	string primaries;
	string strength;
	string title;
	string group = "/>";
} options;

static string xmp_container[] = { "<x:xmpmeta xmlns:x=\"adobe:ns:meta/\" x:xmptk=\"Adobe XMP Core 5.6-c140 79.160451, 2017/05/06-01:08:21        \">\n <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">\n  <rdf:Description rdf:about=\"\"\n    xmlns:crs=\"http://ns.adobe.com/camera-raw-settings/1.0/\"\n   crs:PresetType=\"Look\"\n   crs:Cluster=\"\"\n   crs:UUID=\"", "\"\n   crs:SupportsAmount=\"True\"\n   crs:SupportsColor=\"True\"\n   crs:SupportsMonochrome=\"True\"\n   crs:SupportsHighDynamicRange=\"True\"\n   crs:SupportsNormalDynamicRange=\"True\"\n   crs:SupportsSceneReferred=\"True\"\n   crs:SupportsOutputReferred=\"True\"\n   crs:CameraModelRestriction=\"\"\n   crs:Copyright=\"\"\n   crs:ContactInfo=\"\"\n   crs:Version=\"13.1\"\n   crs:ProcessVersion=\"11.0\"\n", "   crs:ConvertToGrayscale=\"False\"\n   crs:RGBTable=\"", "\"\n   crs:Table_", "=\"", "\"\n   crs:HasSettings=\"True\">\n   <crs:Name>\n    <rdf:Alt>\n     <rdf:li xml:lang=\"x-default\">", "</rdf:li>\n    </rdf:Alt>\n   </crs:Name>\n   <crs:ShortName>\n    <rdf:Alt>\n     <rdf:li xml:lang=\"x-default\"/>\n    </rdf:Alt>\n   </crs:ShortName>\n   <crs:SortName>\n    <rdf:Alt>\n     <rdf:li xml:lang=\"x-default\"/>\n    </rdf:Alt>\n   </crs:SortName>\n   <crs:Group>\n    <rdf:Alt>\n     <rdf:li xml:lang=\"x-default\"", "\n    </rdf:Alt>\n   </crs:Group>\n   <crs:Description>\n    <rdf:Alt>\n     <rdf:li xml:lang=\"x-default\"/>\n    </rdf:Alt>\n   </crs:Description>\n  </rdf:Description>\n </rdf:RDF>\n</x:xmpmeta>\n" };

struct optparse_long longopts[] = {
 {"size", 's' ,OPTPARSE_REQUIRED},
 {"min",  'm'   ,OPTPARSE_REQUIRED},
 {"max",   'x'     ,OPTPARSE_REQUIRED},
 {"amount", 'a'  ,OPTPARSE_REQUIRED},
 {"primaries", 'p' ,OPTPARSE_REQUIRED},
 {"gamut",   'g' ,OPTPARSE_REQUIRED},
 {"strength", 't'  ,OPTPARSE_REQUIRED},
 {"output",  'o' ,OPTPARSE_REQUIRED},
 {"title",  'T',   OPTPARSE_REQUIRED},
 {"group", 'G',   OPTPARSE_REQUIRED},
 {"help",  'h',    OPTPARSE_REQUIRED},
 {0}
};

string get_file_contents(string filename) {
	FILE* fp = fopen(filename.c_str(), "rb");
	if (fp) {
		string contents;
		fseek(fp, 0, SEEK_END);
		contents.resize(ftell(fp));
		rewind(fp);
		fread(&contents[0], 1, contents.size(), fp);
		fclose(fp);
		return(contents);
	}
	throw;
}


void encode(string path, string& outFileName) {
	string* text = new string(get_file_contents(path));
	int32 found1; string title = "";
	if (!options.title.empty()) title = options.title;
	else if ((found1 = text->find("TITLE", 0)) != -1) {
		if (found1 = text->find("\"", found1 + 5) + 1) title = text->substr(found1, text->find("\"", found1) - found1);
		else if (found1 = text->find("'", found1 + 5) + 1) title = text->substr(found1, text->find("'", found1) - found1);
	}
	else title = path.substr(path.find_last_of("/\\") + 1, path.find_last_of(".") - (path.find_last_of("/\\") + 1));
	int32 input_size = stoi(text->substr(text->find("_SIZE", 0) + 5, 3));
	printf("- test encoding back -\nTITLE: %s\nSIZE: %d\n", title.c_str(), input_size);
	double* samples_1 = new double[input_size * input_size * input_size * 3];

	char* points = nullptr;
	for (const char* s = text->c_str(); *s; ++s) if (*s == '\n' && *++s <= '9' && *s >= '0') { points = strdup(s); delete text;  break; }
	for (int32 idx = 0; idx < input_size * input_size * input_size * 3; idx += 3) {
		samples_1[idx] = strtod(points++, &points);
		samples_1[idx + 1] = strtod(points++, &points);
		samples_1[idx + 2] = strtod(points++, &points);
	}


	uint32 size = (input_size > options.size) ? options.size : input_size;
	if (input_size > 32) printf("ACR unsupports LUT>32, resampling enabled\n");
	uint16* nopValue_1 = new uint16[size];
	for (uint32 index = 0; index < size; index++)
		nopValue_1[index] = (index * 0x0FFFF + (size >> 1)) / (size - 1);
	uint32 padding = 16 + size * size * size * 3 * 2 + 28;
	uint8* samples_2 = new uint8[padding];

	uint32 header[4] = { 1,1,3,size };
	memcpy(samples_2, header, 16);
	uint32 colors = 0, gamma = 1; //default sRGB
	if (options.primaries == "Adobe") { colors = 1; gamma = 3; }
	else if (options.primaries == "ProPhoto") { colors = 2; gamma = 2; }
	else if (options.primaries == "P3") { colors = 3; gamma = 1; }
	else if (options.primaries == "Rec2020") { colors = 4; gamma = 4; }
	uint32 footer[3] = { colors,gamma,options.gamut };
	memcpy(samples_2 + 16 + size * size * size * 3 * 2, footer, 12);
	double range[2] = { options.min * 0.01,options.max * 0.01 };
	memcpy(samples_2 + 16 + size * size * size * 3 * 2 + 12, range, 16);

	if (input_size != size) {
		uint16* shrinked = new uint16[size * size * size * 3];
		shrink(samples_1, shrinked, input_size, size);
		delete[] samples_1;

		for (uint32 bIndex = 0, idx, j = 0; bIndex < size; ++bIndex)
			for (uint32 gIndex = 0; gIndex < size; ++gIndex)
				for (uint32 rIndex = 0; rIndex < size; ++rIndex, j += 3) {
					idx = 16 + (rIndex * size * size + gIndex * size + bIndex) * 3 * 2;

					uint16 temp = shrinked[j] - nopValue_1[rIndex];
					memcpy(samples_2 + idx, &temp, 2);

					temp = shrinked[j + 1] - nopValue_1[gIndex];
					memcpy(samples_2 + idx + 2, &temp, 2);


					temp = shrinked[j + 2] - nopValue_1[bIndex];
					memcpy(samples_2 + idx + 4, &temp, 2);

				}
		delete[] shrinked;
	}
	else {
		for (uint32 bIndex = 0, idx, j = 0; bIndex < size; ++bIndex)
			for (uint32 gIndex = 0; gIndex < size; ++gIndex)
				for (uint32 rIndex = 0; rIndex < size; ++rIndex, j += 3) {
					idx = 16 + (rIndex * size * size + gIndex * size + bIndex) * 3 * 2;

					uint16 temp = (int_round((float)samples_1[j] * 65535)) - nopValue_1[rIndex];
					memcpy(samples_2 + idx, &temp, 2);

					temp = (int_round((float)samples_1[j + 1] * 65535)) - nopValue_1[gIndex];
					memcpy(samples_2 + idx + 2, &temp, 2);


					temp = (int_round((float)samples_1[j + 2] * 65535)) - nopValue_1[bIndex];
					memcpy(samples_2 + idx + 4, &temp, 2);

				}
		delete[] samples_1;
	}
	delete[] nopValue_1;

	printf("tot: %d\n", padding);
#ifdef DEBUG
	FILE* f_5 = fopen("C:\\Users\\Michele\\Downloads\\outputarray.txt", "w+");
	for (uint32 i = 0, j = 0, k = 0; i < padding; ++i) {
		if (i >= 16 && i < padding - 28) {
			if (j % 6 == 0) { fputs(("\n"), f_5); j = 0; }
			j++;
		}
		else if (i >= padding - 18) {
			if (j % 8 == 0) { fputs(("\n"), f_5); j = 0; }
			j++;
		}
		else {
			if (k == 4) { fputs(("\n"), f_5); k = 0; }
			k++;
		}
		fprintf(f_5, "%d ", samples_2[i]);
	}
	fclose(f_5);
#endif

	uint8* block1_1 = samples_2;
	uint32 uncompressedSize_1 = padding;
	uint32 safeCompressedSize = (uncompressedSize_1 | uncompressedSize_1 >> 8) + 64;
	printf("\n%s %d \n%s %d\n", "uncompressedSize_1:", uncompressedSize_1, "safeCompressedSize:", safeCompressedSize);

	string MD5 = md5(block1_1, uncompressedSize_1);
	printf("MD5: %s\n", MD5.c_str());
	string UUID = MD5 + std::to_string(time_t(time(NULL)));
	UUID = md5(UUID, UUID.length());
	printf("UUID(aka MD5 of MD5+TIME): %s\n", UUID.c_str());

	uint8* dPtr_1 = new uint8[safeCompressedSize + 4];
	memcpy(dPtr_1, &uncompressedSize_1, 4);
	uLongf dCount = safeCompressedSize;
	int32 zResult_1 = compress2(dPtr_1 + 4, &dCount, block1_1, uncompressedSize_1, Z_DEFAULT_COMPRESSION);
	printf("%s %d\n", "zResult_1:", zResult_1);
	delete[] samples_2;
	uint32 compressedSize_1 = (uint32)dCount + 4;
#ifdef DEBUG
	FILE* f_1 = fopen("C:\\Users\\Michele\\Downloads\\outputencoded.txt", "w+");
	for (uint32 i = 0; i < safeCompressedSize + 4; ++i)
		fputs((std::to_string(dPtr_1[i]) + " ").c_str(), f_1);
	fclose(f_1);
#endif

	printf("%s %d\n", "compressedSize_1:", compressedSize_1);
	uLongf destLen_1 = uncompressedSize_1;
	uint8* block3_1 = new uint8[uncompressedSize_1];
	int32 zResult_2 = uncompress(block3_1, &destLen_1, dPtr_1 + 4, compressedSize_1 - 4);
	printf("%s %d", "zResult_2:", zResult_2);
#ifdef DEBUG
	FILE* f_2 = fopen("C:\\Users\\Michele\\Downloads\\outputencoded_1.txt", "w+");
	for (uint32 i = 0; i < uncompressedSize_1; ++i)
		fputs((std::to_string(block3_1[i]) + " ").c_str(), f_2);
	fclose(f_2);
#endif
	delete[] block3_1;

	static const char* kEncodeTable = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ.-:+=^!/*?`'|()[]{}@%$#";
	uint32 safeEncodedSize = compressedSize_1 + (compressedSize_1 >> 2) + (compressedSize_1 >> 6) + 16;

	uint8* sPtr_1 = dPtr_1;
	sPtr_1[compressedSize_1] = 0;
	sPtr_1[compressedSize_1 + 1] = 0;
	sPtr_1[compressedSize_1 + 2] = 0;
	uint8* dPtr_2 = new uint8[safeEncodedSize];
	uint32 k = 0;
	for (uint32 i = 0, x; compressedSize_1; i += 4) {
		x = sPtr_1[i + 0] | sPtr_1[i + 1] << 8 | sPtr_1[i + 2] << 16 | sPtr_1[i + 3] << 24;
		for (uint32 j = 0; j < 5; ++j, x /= 85) {
			dPtr_2[k++] = kEncodeTable[x % 85];
			if (j > 0 && !--compressedSize_1) break;
		}
	}
	delete[] dPtr_1;

	FILE* f_6 = fopen(outFileName.c_str(), "w+");
	string assembled = xmp_container[0] + UUID + xmp_container[1] + options.strength + xmp_container[2] + MD5 + xmp_container[3] + MD5 + xmp_container[4];
	fwrite(assembled.c_str(), 1, assembled.size(), f_6);
	fwrite(dPtr_2, 1, k, f_6);
	assembled = options.amount + xmp_container[5] + title + xmp_container[6] + options.group + xmp_container[7];
	fwrite(assembled.c_str(), 1, assembled.size(), f_6);
	fclose(f_6);

	delete[] dPtr_2;
	printf("\n%s %d\n%s %d\n", "safeEncodedSize:", safeEncodedSize, "true EncodedSize:", k);
}

void decode(string path, string& outFileName) {

	uint32 compressedSize = 0;
	string* block1 = new string(get_file_contents(path));
	uint32 found = block1->find("Name>\n    <rdf:Alt>\n     <rdf:li xml:lang=\"x-default\">") + 54;
	if (found == 53) found = block1->find("Name>\r\n    <rdf:Alt>\r\n     <rdf:li xml:lang=\"x-default\">") + 56;
	string title = block1->substr(found, block1->find("</", found) - found);
	found = block1->find("=\"", block1->find(":RGBTable=\"") + 11) + 2;
	*block1 = block1->substr(found, (block1->find("\"", found)) - found);
	uint32 encodedSize = block1->length();

	uint32 maxCompressedSize = (encodedSize + 4) / 5 * 4;
	uint8* dPtr = new uint8[maxCompressedSize];
	printf("- test decoding -\n%s %d\n", "maxCompressedSize:", maxCompressedSize);

	static const uint8 kDecodeTable[96] = { 0xFF,0x44,0xFF,0x54,0x53,0x52,0xFF,0x49,0x4B,0x4C,0x46,0x41,0xFF,0x3F,0x3E,0x45,0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x40,0xFF,0xFF,0x42,0xFF,0x47,0x51,0x24,0x25,0x26,0x27,0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2E,0x2F,0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3A,0x3B,0x3C,0x3D,0x4D,0xFF,0x4E,0x43,0xFF,0x48,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D,0x1E,0x1F,0x20,0x21,0x22,0x23,0x4F,0x4A,0x50,0xFF,0xFF };

	uint32 phase = 0, value;

	for (const char* sPtr = block1->c_str(); *sPtr; sPtr++) {
		uint8 e = *sPtr;
		if (e < 32 || e > 127) continue;
		uint32 d = kDecodeTable[e - 32];
		if (d > 85) continue;
		phase++;
		switch (phase) {
		case 1: value = d; break;
		case 2: value += d * 85; break;
		case 3: value += d * (85 * 85); break;
		case 4: value += d * (85 * 85 * 85); break;
		case 5: value += d * (85 * 85 * 85 * 85);
			memcpy(dPtr + compressedSize, &value, 4);
			compressedSize += 4;
			phase = 0;
			break;
		}
	}
	delete block1;
	uint32 current_idx = compressedSize;
	printf("phase: %d", phase);
	switch (phase) {
	case 4: dPtr[current_idx + 2] = value >> 16; compressedSize++;
	case 3: dPtr[current_idx + 1] = value >> 8;  compressedSize++;
	case 2: dPtr[current_idx] = value;       compressedSize++;
	}

	uint32 uncompressedSize = dPtr[0] | dPtr[1] << 8 | dPtr[2] << 16 | dPtr[3] << 24;

	printf("\n%s %d\n%s %d\n%s %d\n%s %d\n", "encodedSize:", encodedSize, "compressedSize:", compressedSize, "uncompressedSize:", uncompressedSize, "compressedSize-4:", compressedSize - 4);
#ifdef DEBUG
	FILE* f = fopen("C:\\Users\\Michele\\Downloads\\outputdecoded.txt", "w+");
	for (uint32 i = 0; i < compressedSize; ++i)
		fprintf(f, "%d ", dPtr[i]);
	fclose(f);
#endif

	uint8* block3 = new uint8[uncompressedSize];
	uLongf destLen = uncompressedSize;
	int32 zResult = uncompress(block3, &destLen, dPtr + 4, compressedSize - 4);
	printf("\n%s %d\n", "zResult:", zResult);
	delete[] dPtr;

#ifdef DEBUG
	FILE* f_3 = fopen("C:\\Users\\Michele\\Downloads\\final.txt", "w+");
	for (uint32 i = 0, j = 0, k = 0; i < uncompressedSize; ++i) {
		if (i >= 16 && i < uncompressedSize - 28) {
			if (j % 6 == 0) { fputs(("\n"), f_3); j = 0; }
			j++;
		}
		else if (i >= uncompressedSize - 18) {
			if (j % 8 == 0) { fputs(("\n"), f_3); j = 0; }
			j++;
		}
		else {
			if (k == 4) { fputs(("\n"), f_3); k = 0; }
			k++;
		}
		fprintf(f_3, "%d ", block3[i]);
	}
	fclose(f_3);
#endif

	printf("MD5: %s\n", md5(block3, uncompressedSize).c_str());

	const uint8 fDivisions = block3[12];
	uint16* nopValue = new uint16[fDivisions];
	for (uint32 index = 0; index < fDivisions; index++)
		nopValue[index] = (index * 0x0FFFF + (fDivisions >> 1)) / (fDivisions - 1);

	FILE* f_4 = fopen(outFileName.c_str(), "w+");
	fprintf(f_4, "TITLE \"%s\"\nDOMAIN_MIN 0 0 0\nDOMAIN_MAX 1 1 1\nLUT_3D_SIZE %d\n", title.c_str(), fDivisions);

	for (uint32 rIndex = 0, idx; rIndex < fDivisions; ++rIndex)
		for (uint32 gIndex = 0; gIndex < fDivisions; ++gIndex)
			for (uint32 bIndex = 0; bIndex < fDivisions; ++bIndex) {
				idx = (rIndex + gIndex * fDivisions + bIndex * fDivisions * fDivisions) * 6;
				fprintf(f_4, "%.9f %.9f %.9f\n",
					((uint16)((block3[16 + idx + 0] | block3[16 + idx + 1] << 8) + nopValue[bIndex])) / 65535.0f,
					((uint16)((block3[16 + idx + 2] | block3[16 + idx + 3] << 8) + nopValue[gIndex])) / 65535.0f,
					((uint16)((block3[16 + idx + 4] | block3[16 + idx + 5] << 8) + nopValue[rIndex])) / 65535.0f
				);
			}
	fclose(f_4);
	delete[] nopValue;
	delete[] block3;
}

int main(int argc, char** argv) {
	std::vector<string> inputFiles;
	struct optparse optargs;
	optparse_init(&optargs, argv);
	for (int32 c = optparse_long(&optargs, longopts, NULL); c != -1; c = optparse_long(&optargs, longopts, NULL)) {
		switch (c) {

		case 'T':
			options.title = optargs.optarg;
			break;

		case 'G':
			options.group = ">" + string(optargs.optarg) + "</rdf:li>";
			break;

		case 's':
			if (atoi(optargs.optarg) > 1 && atoi(optargs.optarg) < 32)
				options.size = atoi(optargs.optarg);
			break;

		case 'm':
			if (atoi(optargs.optarg) >= 0 && atoi(optargs.optarg) <= 100)
				options.min = atoi(optargs.optarg);
			break;

		case 'x':
			if (atoi(optargs.optarg) >= 100 && atoi(optargs.optarg) <= 200)
				options.max = atoi(optargs.optarg);
			break;

		case 'a':

			if (atoi(optargs.optarg) >= 0 && atoi(optargs.optarg) <= 200 && atoi(optargs.optarg) != 100) {
				string val = std::to_string(atoi(optargs.optarg) * 0.01);
				val.erase(val.find_last_not_of('0') + 1, string::npos).erase(val.find_last_not_of('.') + 1, string::npos);
				options.amount = "\"\n   crs:RGBTableAmount=\"" + val;
			}
			break;

		case 'p':
			options.primaries = optargs.optarg;
			break;

		case 'g':
			if (strcmp(optargs.optarg, "extend") == 0)
				options.gamut = 1;
			break;

		case 't':
			if (strcmp(optargs.optarg, "medium") == 0) options.strength = "   crs:ToneMapStrength=\"1\"\n";
			else if (strcmp(optargs.optarg, "high") == 0) options.strength = "   crs:ToneMapStrength=\"2\"\n";
			break;

		case 'o':
			options.outFileName = optargs.optarg;
			break;

		case 'h':
		case '?':
			printf("\nXMPconverter.exe [-h] [--output] [--size] [--min] [--max] [--amount] [--gamut]\n                 [--primaries] [--strength] [--title] [--group] FILE/FILES/FOLDER\n\nConvert XMP to CUBE and vice versa, written by Michele Renzullo.\nThanks to Adobe for DNG SDK.\n\noptional arguments:\n  --help, -h           show this help message and exit\n  --output, -o         output file, files or folder(if batch)\n  --size, -s           size aka number of samples: <=32\n  --min, -m            min amount of XMP: from 0(default) to 100\n  --max, -x            max amount of XMP: from 100 to 200(default)\n  --amount, -a         default amount of XMP: from 0 to 200\n  --gamut, -g          clip(default) or extend\n  --primaries, -p      sRGB(default), Adobe, ProPhoto, P3, Rec2020\n  --strength, -t       tone map strength: low(default), medium, high\n  --title, -T          title of output\n  --group, -G           group title of xmp output\n");
			return 0;

		}
	}

	if (char* inputFiles_char = optparse_arg(&optargs)) {
		for (; inputFiles_char; inputFiles_char = optparse_arg(&optargs)) {
			string inputFile = inputFiles_char;
			if (dirExists(inputFile))
				read_directory(inputFile + "/", inputFiles);
			else inputFiles.push_back(inputFile);
		}

		if (!mkpath(options.outFileName)) { printf("can't write here\n"); return 0; }
		bool outputIsDir = false;
		if (dirExists(options.outFileName)) outputIsDir = true;

		if (inputFiles.size() > 1) {
			if (!options.title.empty()) {
				options.title = "";
				printf("WARNING: multiple inputs but only one title\n");
			}
			if (!options.outFileName.empty() && !outputIsDir) {
				mkpath(options.outFileName + "/");
				outputIsDir = true;
			}
		}

		string ext[] = { "xmp","cube" };
		auto t1 = std::chrono::high_resolution_clock::now();
		//tbb::parallel_for_each(inputFiles.begin(), inputFiles.end(), [&](string& inputFile) {
		for_each(std::execution::par, inputFiles.begin(), inputFiles.end(), [&](string& inputFile) {
		//#pragma omp parallel for
		//for (const auto& inputFile : inputFiles) {

			if (fileExists(inputFile)) {
				string filecheck = inputFile.substr(inputFile.find_last_of(".") + 1);
				transform(filecheck.begin(), filecheck.end(), filecheck.begin(), tolower);

				string outFileName;
				if (filecheck == ext[0] xor filecheck == ext[1]) {
					if (outputIsDir) {
						outFileName = options.outFileName + "/" + inputFile.substr(inputFile.find_last_of("/\\") + 1, inputFile.find_last_of(".") - (inputFile.find_last_of("/\\") + 1)) + ".";
						outFileName += (filecheck == ext[0]) ? ext[1] : ext[0];
					}
					else if (!options.outFileName.empty()) outFileName = options.outFileName;
					else {
						outFileName = inputFile.substr(inputFile.find_last_of("/\\") + 1, inputFile.find_last_of(".") - (inputFile.find_last_of("/\\") + 1)) + ".";
						outFileName += (filecheck == ext[0]) ? ext[1] : ext[0];
					}
					(filecheck == ext[0]) ? decode(inputFile, outFileName) : encode(inputFile, outFileName);
				}
			}
		});
		printf("\ntime: %lld\n", std::chrono::duration_cast<std::chrono::seconds>(std::chrono::high_resolution_clock::now() - t1).count());
	}
	else printf("missing input");
	printf("\n");
}
