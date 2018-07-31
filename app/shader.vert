#version 410 core

layout (location = 0) in vec4 aPos;
layout (location = 1) in vec4 aColor;

out vec4 ourColor;

void main()
{
   gl_Position = aPos;
   ourColor = aColor;
}
