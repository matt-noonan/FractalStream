#version 410 core

layout (location = 0) in vec2 pos;

out vec4 FragPos;

void main()
{
   FragPos = vec4(pos, 0.0, 1.0);
   gl_Position = FragPos;
}