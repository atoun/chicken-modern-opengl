all : glew.so glmisc.so tga.so

tga.so : tga.scm src/tga.scm
	csc tga.scm -shared -o tga.so -J

glew.so : glew.scm src/glew.scm
	csc glew.scm -shared -o glew.so -J -lGLEW

glmisc.so : glmisc.scm src/glmisc.scm
	csc glmisc.scm -shared -o glmisc.so -J -lGL -lGLEW -lglut

clean :
	rm -f *.so *.import.scm
