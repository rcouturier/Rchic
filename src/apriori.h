#ifndef APRIORI
#define APRIORI


#include <iostream>
#if defined __GNUC__ || defined __APPLE__
#include <ext/hash_map>
#else
#include <hash_map>
#endif
#include <stdio.h>
#include <math.h>


#include <fstream>

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <assert.h>
#ifndef NIMAPFN
#define NIMAPFN
#endif
#include "symtab.h"
#include "tfscan.h"
#include "istree.h"
#ifdef STORAGE
#include "storage.h"
#endif
#include <unordered_map>

//#if !defined (STLPORT) || defined(_STLP_USE_NAMESPACES)
//using namespace std;
//#endif
using namespace std;

class truc{
public:
	inline truc() {name=0;size=0;}
	inline truc(const char *s) {size=strlen(s);name=new char[size+1];strcpy(name,s);}
	inline truc(const int s1, const int s2) {
		char tmp[20];
		sprintf(tmp,"%i %i",s1,s2);
		size=strlen(tmp);
		name=new char[size+1];
		strcpy(name,tmp);
	}
	inline truc(const truc &t) {size=strlen(t.name);name=new char[size+1];strcpy(name,t.name);}
	inline ~truc() {if(name) delete []name;size=0;}
	inline char* getname() const {return name;}
	inline int getsize() const {return size;}
private:
	char *name;
	int size;
};





struct eqstr { 
	bool operator()(const truc& s1, const truc& s2) const {
		return strcmp(s1.getname(), s2.getname()) == 0; 
	} 
}; 

struct hashstr
{
	inline size_t operator()( const truc& a ) const
	{

		unsigned long h = 0; 
		char *s=a.getname();
		for ( ; *s; ++s)
			h = 5*h + *s;
  
		return size_t(h);
		
	}
};



typedef unordered_map<truc,double, hashstr, eqstr > doublemaptype;
typedef unordered_map<truc,int, hashstr, eqstr > intmaptype;




int monmain (int argc, char *argv[]);

#endif
