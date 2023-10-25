#define STB_IMAGE_WRITE_IMPLEMENTATION
#include <stdio.h>
#include "stb_image_write.h"

int write_png(const char* filename, int width, int height, int comp, const char* data)
{
    return stbi_write_png(filename, width, height, comp, (void*)data, width * comp);
}
