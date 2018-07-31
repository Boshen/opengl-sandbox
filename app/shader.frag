#version 410 core

in vec4 ourColor;
uniform float time;

out vec4 FragColor;

void main()
{
  FragColor = ourColor * vec4(cos(time) + 0.5, -1 * sin(time) + 0.5, sin(time) / 2 + 0.5, 1);
}
