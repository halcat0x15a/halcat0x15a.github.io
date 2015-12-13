all: c.out go.out cpp.out java.jar cs.exe scala.jar hs.out
	./c.out
	./go.out
	./cpp.out
	java -jar java.jar
	mono cs.exe
	scala scala.jar
	./hs.out
	ruby main.rb
	python main.py

c.out: main.c
	gcc -o $@ $<

go.out: main.go
	go build -o $@ $<

cpp.out: main.cpp
	g++ -std=c++11 -o $@ $<

java.jar: Main.java
	javac $<
	echo "Main-Class: Main" > MANIFEST.MF
	jar -cfm $@ MANIFEST.MF Main.class
	rm MANIFEST.MF Main.class

cs.exe: Main.cs
	mcs -out:$@ $<

scala.jar: Main.scala
	scalac $<
	echo "Main-Class: Main" > MANIFEST.MF
	jar -cfm $@ MANIFEST.MF *.class
	rm MANIFEST.MF *.class

hs.out: Main.hs
	ghc -o $@ $<
	rm Main.o Main.hi

clean:
	rm *.out *.jar *.exe
