#define STB_IMAGE_IMPLEMENTATION
#include <stdio.h>
#include "stb_image.h"

uint8_t* load_image(const char* filename, int* width, int* height, int* comp)
{
    uint8_t* result = stbi_load(filename, width, height, comp, 4);
    return result;
}

void free_image(uint8_t* image) 
{
    stbi_image_free(image);
}
