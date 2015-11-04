#version 330 core

in vec3 vNormal;

out vec4 color;

void main(void) {

  color = vec4(abs(vNormal), 1.0);

}
