#include <stdio.h>
#include <string.h>

#include <GL/glew.h>
#include <GL/gl.h>
#include <GLFW/glfw3.h>

const char* VERTEX_SHADER =
    "#version 330                            \n"
    "layout(location = 0) in vec4 vtx_pos  ; \n"
    "layout(location = 1) in vec2 vtx_txpos; \n"
    "out vec2 texcoord;                      \n"
    "void main() {                           \n"
    "   texcoord = vtx_txpos;                \n"
    "   gl_Position = vtx_pos;               \n"
    "}                                       \n";

const char* FRAGMENT_SHADER =
    "#version 330                            \n"
    "uniform sampler2D tex;                  \n"
    "in vec2 texcoord;                       \n"
    "layout(location = 0) out vec4 FragColor;\n"
    "void main() {                           \n"
    "   FragColor = texture(tex, texcoord);  \n"
    "}                                       \n";

GLFWwindow* window;
GLuint shader_program, vertex_shader, fragment_shader;
GLuint vao, vbo, ibo;
GLint texture_location;
GLuint texture;

char* buffer;
int width, height;

void error_callback(int error, const char* description) 
{
    fprintf(stderr, "Error: %s (%d)\n", description, error);
}

void framebuffer_size_callback(GLFWwindow* window, int width, int height)
{
    glViewport(0, 0, width, height);
}

int window_init(char* buf, int w, int h)
{
    buffer = buf;
    width = w;
    height = h;

    glfwSetErrorCallback(error_callback);
    if (!glfwInit()) 
    {
        return -1;
    }

    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);

    int resx = w, resy = h;
    window = glfwCreateWindow(resx, resy, "Dolquin", NULL, NULL);
    if (!window) 
    { 
        glfwTerminate();
        return -2;
    }

    glfwMakeContextCurrent(window);

    if (glewInit() != GLEW_OK) 
    {
        fprintf(stderr, "Error initializing GLEW.\n");
        return -1;
    }

    glfwSetFramebufferSizeCallback(window, framebuffer_size_callback);

    const char* source;
    int length;
    GLint status;

    source = VERTEX_SHADER;
    length = strlen(VERTEX_SHADER);
    vertex_shader = glCreateShader(GL_VERTEX_SHADER);
    glShaderSource(vertex_shader, 1, &source, &length);
    glCompileShader(vertex_shader);
    // Todo check compilation
    glGetShaderiv(vertex_shader, GL_COMPILE_STATUS, &status);
    if(status == GL_FALSE) 
    {
        fprintf(stderr, "Shader compilation failed.");
        return -1;
    }

    source = FRAGMENT_SHADER;
    length = strlen(FRAGMENT_SHADER);
    fragment_shader = glCreateShader(GL_FRAGMENT_SHADER);
    glShaderSource(fragment_shader, 1, &source, &length);
    glCompileShader(fragment_shader);
    // Todo check compilation
    glGetShaderiv(fragment_shader, GL_COMPILE_STATUS, &status);
    if(status == GL_FALSE) 
    {
        fprintf(stderr, "Shader compilation failed.");
        return -1;
    }

    shader_program = glCreateProgram();

    glAttachShader(shader_program, vertex_shader  );
    glAttachShader(shader_program, fragment_shader);

    glLinkProgram(shader_program);
    glGetProgramiv(shader_program, GL_LINK_STATUS, &status);
    if(status == GL_FALSE) 
    {
        fprintf(stderr, "Could not link shader.");
        return -1;
    }

    texture_location = glGetUniformLocation(shader_program, "tex");

    glGenVertexArrays(1, &vao);
    glBindVertexArray(vao);

    glGenBuffers(1, &vbo);
    glBindBuffer(GL_ARRAY_BUFFER, vbo);

    GLfloat vertexData[] = {
    //  x     y     z     u     v
       1.0f, 1.0f, 0.0f, 1.0f, 1.0f,
      -1.0f, 1.0f, 0.0f, 0.0f, 1.0f,
       1.0f,-1.0f, 0.0f, 1.0f, 0.0f,
      -1.0f,-1.0f, 0.0f, 0.0f, 0.0f,
    };

    glBufferData(GL_ARRAY_BUFFER, sizeof(GLfloat)*4*5, vertexData, GL_STATIC_DRAW);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 5*sizeof(GLfloat), (char*)0 + 0*sizeof(GLfloat));
 
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 5*sizeof(GLfloat), (char*)0 + 3*sizeof(GLfloat));
    
    
    // generate and bind the index buffer object
    glGenBuffers(1, &ibo);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ibo);
            
    GLuint indexData[] = {
        0,1,2, // first triangle
        2,1,3, // second triangle
    };

    // fill with data
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, sizeof(GLuint)*2*3, indexData, GL_STATIC_DRAW);
    
    // "unbind" vao
    glBindVertexArray(0);
    
    // generate texture
    glGenTextures(1, &texture);

    // bind the texture
    glBindTexture(GL_TEXTURE_2D, texture);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
}

int window_update() 
{
    if (glfwWindowShouldClose(window)) 
    {
        return 0;
    }

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, buffer);

    glfwPollEvents();

    glClear(GL_COLOR_BUFFER_BIT);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glUseProgram(shader_program);
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, texture);
    glUniform1i(texture_location, 0);
    glBindVertexArray(vao);
    glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_INT, 0);

    GLenum error = glGetError();
    if(error != GL_NO_ERROR) {
        fprintf(stderr, "%d", error);
    }

    if (glfwGetKey(window, GLFW_KEY_ESCAPE))
    {
        glfwSetWindowShouldClose(window, GLFW_TRUE);
    }

    glfwSwapBuffers(window);

    return 1;
}

void window_destroy()
{
    glDeleteTextures(1, &texture);
    
    glDeleteVertexArrays(1, &vao);
    glDeleteBuffers(1, &vbo);
    glDeleteBuffers(1, &ibo);
    
    glDetachShader(shader_program, vertex_shader);	
    glDetachShader(shader_program, fragment_shader);
    glDeleteShader(vertex_shader);
    glDeleteShader(fragment_shader);
    glDeleteProgram(shader_program);

    glfwTerminate();
}