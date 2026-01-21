#version 410 core

uniform vec2 uMouse;
uniform vec2 uDiameter;
uniform vec2 uCenter;
uniform sampler1D uTexture;

in vec4 FragPos;

out vec4 FragColor;

// Complex Addition, Subtraction, and Additive Inverse
vec2 _cAdd(vec2 a, vec2 b) {return a + b;}
vec2 _cSub(vec2 a, vec2 b) {return a - b;}
vec2 _cOpp(vec2 a) {return -a;}

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
      
      // (z^2-1+c-c^3) / (z^2-c^2)      
      z = _cDiv(_cMul(z, z) - vec2(1.0, 0.0) + c - _cMul(c, _cMul(c, c)), _cMul(z, z) - _cMul(c, c));

      // z^2 + c
      // z = _cAdd(_cMul(z, z), c);
   }

   
   // if (length(uMouse - c) < 1e-3) {
   //    FragColor = vec4(0.0, 0.0, 1.0, 1.0);
   // }
}  
