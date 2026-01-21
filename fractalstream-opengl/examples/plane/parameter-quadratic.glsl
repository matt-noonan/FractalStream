#version 410 core

uniform vec2 uMouse;
uniform vec2 uDiameter;
uniform vec2 uCenter;
uniform sampler1D uTexture;

in vec4 FragPos;

out vec4 FragColor;

// Complex Multiplication
vec2 _cMul(vec2 a, vec2 b) {
   return vec2(a.x * b.x - a.y * b.y, a.x * b.y + a.y * b.x);
}

// Complex Division
vec2 _cDiv(vec2 a, vec2 b) {
   return vec2(a.x * b.x + a.y * b.y, a.y * b.x - a.x * b.y) / (b.x * b.x + b.y * b.y);
}

void main() {
   vec2 c = uCenter + FragPos.xy * uDiameter / 2.0;
   vec2 z = vec2(0.0);
   
   FragColor = texture(uTexture, 0.5);
   
   for (int iter = 0; iter < 300; iter++) {
      if (length(z) > 100.0) {
         float depth = fract((float(iter) - log2(log(length(z)))) / 32.0);
         FragColor = texture(uTexture, depth);
         break;
      }
      
      // z^2+c
      z = _cMul(z, z) + c;
   }
}  
