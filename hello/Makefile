run: all
	./main_c.out
	./main_go.out
	./main_cpp.out
	java -jar main_java.jar
	mono main_cs.exe
	scala main_scala.jar
	./main_hs.out
	ruby main.rb
	python main.py

all: main_c.out main_go.out main_cpp.out main_java.jar main_cs.exe main_scala.jar main_hs.out

main_c.out: main.c
	gcc -o $@ $<

main_go.out: main.go
	go build -o $@ $<

main_cpp.out: main.cpp
	g++ -std=c++11 -o $@ $<

main_java.jar: Main.java
	javac $<
	echo "Main-Class: Main" > MANIFEST.MF
	jar -cfm $@ MANIFEST.MF *.class

main_cs.exe: Main.cs
	mcs -out:$@ $<

main_scala.jar: Main.scala
	scalac $<
	echo "Main-Class: Main" > MANIFEST.MF
	jar -cfm $@ MANIFEST.MF *.class

main_hs.out: Main.hs
	ghc -o $@ $<

clean:
	rm -f *.out *.class MANIFEST.MF *.jar *.exe *.o *.hi
