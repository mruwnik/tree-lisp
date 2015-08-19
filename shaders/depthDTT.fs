#version 120

// Ouput data
float fragmentdepth;


void main(){
	fragmentdepth = gl_FragCoord.z;
}