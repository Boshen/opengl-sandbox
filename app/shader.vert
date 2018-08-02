#version 410 core

layout (location = 0) in vec4 aPos;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

out vec4 ourColor;

void main()
{
   gl_Position = projection * view * model * aPos;
   ourColor = vec4(aPos.x, aPos.y, aPos.z, 1);
}
