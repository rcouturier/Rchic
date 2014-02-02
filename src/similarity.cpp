#include <iostream>
#include <stdio.h>
#include <math.h>


#include <fstream>

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>
#include <R.h>
#include <Rinternals.h>

extern "C"{

void convolve(double *a, int *na, double *b, int *nb, double *ab)
{
    int nab = *na + *nb - 1;

    for(int i = 0; i < nab; i++)
        ab[i] = 0.0;
    for(int i = 0; i < *na; i++)
        for(int j = 0; j < *nb; j++)
            ab[i + j] += a[i] * b[j];
}


/* second version */
SEXP myout(SEXP x, SEXP y)
{
    int nx = length(x), ny = length(y);
    SEXP ans = PROTECT(allocMatrix(REALSXP, nx, ny));
    double *rx = REAL(x), *ry = REAL(y), *rans = REAL(ans);

    for(int i = 0; i < nx; i++) {
  double tmp = rx[i];
	for(int j = 0; j < ny; j++)
	    rans[i + nx*j] = tmp * ry[j];
    }

    SEXP dimnames = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(dimnames, 0, getAttrib(x, R_NamesSymbol));
    SET_VECTOR_ELT(dimnames, 1, getAttrib(y, R_NamesSymbol));
    setAttrib(ans, R_DimNamesSymbol, dimnames);
    UNPROTECT(2);
    return ans;
}

/* second version */
SEXP myout2(SEXP x, SEXP y)
{
    int nx = length(x), ny = length(y);
    SEXP ans = PROTECT(allocMatrix(REALSXP, nx, ny));
    double *rx = REAL(x), *ry = REAL(y), *rans = REAL(ans);

Rprintf("%d %d \n",nx,ny);

    for(int i = 0; i < nx; i++) {
  double tmp = rx[i];
  for(int j = 0; j < ny; j++)
	    rans[i + nx*j] = tmp * ry[j];
    }

    SEXP dimnames = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(dimnames, 0, getAttrib(x, R_NamesSymbol));
    SET_VECTOR_ELT(dimnames, 1, getAttrib(y, R_NamesSymbol));
    setAttrib(ans, R_DimNamesSymbol, dimnames);
    UNPROTECT(2);
    
    
    char buffer[100];
  
    /*SEXP e = allocVector(STRSXP, nx);
    for (int i = 0;i<nx;i++){
      sprintf(buffer, "val %d", i);
      SEXP s = mkChar(buffer);
      SET_STRING_ELT(e, i, s);
    }
    
    return e;   
    */
    
     SEXP alist = R_NilValue;
    for (int i = 0;i<nx;i++){
  PROTECT(alist);
        sprintf(buffer, "val %d", i);
	alist = CONS(mkString(buffer), alist);
	UNPROTECT(1);
    }
    
    return alist;
    
}



SEXP myout3(SEXP x)
{
  
  if(!isReal(x))
    error("name is not a single string");
  Rprintf("value is %f\n", REAL(x)[0]);
  
  return R_NilValue;  
    
}

SEXP myout4(SEXP mat)
{
  
  if(!isMatrix(mat))
    error("matrix needed");
  
  SEXP names=getAttrib(mat, R_DimNamesSymbol);
  SEXP v1= VECTOR_ELT(names, 0);
  Rprintf("is string %d %d\n",isString(v1),isNewList(v1));
  
  //char *s=(CHAR(STRING_ELT(names, 0)));
  Rprintf("name %s\n",CHAR(STRING_ELT(v1, 1)));
    
  SEXP dimnames = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(dimnames, 0, getAttrib(mat, R_DimNamesSymbol));
  SET_VECTOR_ELT(dimnames, 1, getAttrib(mat, R_DimSymbol));
  UNPROTECT(1);  
  return dimnames;  
    
}

void GenericPair(int u,int v,int p,int q,int &maxx, int& maxy, double **Index, int **taby)
{
  int l,t;
	double max=-1;
	double ind;
	for (l=1;l<=p;l++)
		for (t=1;t<=q;t++)
		{
		  ind=Index[taby[u][l]][taby[v][t]];
			if(ind>max) 
			{
				maxx=(int)taby[u][l];
				maxy=(int)taby[v][t];
				max=ind;
			}
		}
}




SEXP similarity(SEXP similarity_matrix) {
  if(!isMatrix(similarity_matrix))
    error("matrix needed");
  
  int nb_col=INTEGER(getAttrib(similarity_matrix, R_DimSymbol))[0];
  SEXP list_names=getAttrib(similarity_matrix, R_DimNamesSymbol);
  SEXP variables= VECTOR_ELT(list_names, 0);
  
  
  
  Rprintf("Nb col %d\n",nb_col);
  
  int i,j,k,u,v;
  int x,y;
  double max;
  bool max_found;
  
  int *level= new int[nb_col];
  int *Item=new int[nb_col];
  for(i=0;i<nb_col;i++)
    Item[i]=1;
  int *tabe=new int[nb_col];
  int *tabo=new int[nb_col];
  int *tabb=new int[nb_col];
  int *tabz=new int[nb_col];
  int *tabee=new int[nb_col];
  int **taby=new int*[nb_col];
  for(int i;i<nb_col;i++)
    taby[i]=new int[nb_col];
  for(i=0;i<nb_col;i++)
  	for(j=0;j<nb_col;j++) taby[i][j]=0;
    
  char **cc=new char*[nb_col];
	char **cl=new char*[nb_col];
	for(i=0;i<nb_col;i++) 
	{
		cc[i]=0;
		cl[i]=0;
	}
  
  for (i=0;i<nb_col;i++)
	{
		if(Item[i]) taby[i][1]=i;
		tabe[i]=1;
    int length=strlen(CHAR(STRING_ELT(variables, i)));
		cc[i]=new char[10];	//it is only to have the number of the variable
		cl[i]=new char[length+1];
		sprintf(cc[i],"%i",i+1);
		sprintf(cl[i],"%s",CHAR(STRING_ELT(variables, i)));
	}
  
  int r=0;
  for(i=0;i<nb_col;i++) 
    r+=Item[i];
	int f=0;
	
    

  int *AlreadyInClasse = new int[nb_col];
	for(i=0;i<nb_col;i++) AlreadyInClasse[i]=-999999;
	
  int  *GenPairX = new int[nb_col];
  int  *GenPairY = new int[nb_col];
  
	int	*Terminal = new int[nb_col];
	for(i=0;i<nb_col;i++) Terminal[i]=1;
	
  
	int *LevelX=new int[r];
	for(i=0;i<r;i++) LevelX[i]=-1;
		
	int *LevelY=new int[r];
	for(i=0;i<r;i++) LevelY[i]=-1;

  double **CurIndex = new double*[nb_col];
  for(i=0;i<nb_col;i++)
    CurIndex[i] = new double[nb_col];

  double *val_mat=REAL(similarity_matrix);
  double **Index= new double*[nb_col];
  for(i=0;i<nb_col;i++)
    Index[i] = new double[nb_col];


  //to test how to get data for the matrix and put data into Index
  for(i=0;i<nb_col;i++) {
    for(j=0;j<nb_col;j++) {
      Index[i][j]=val_mat[j*nb_col+i];  //element in the matrix seems to be transposed
      Rprintf("%f ",Index[i][j]);   
    }
    Rprintf("\n");
  }


  do
  {
		for (u=0;u<nb_col-1;u++)
			for (v=u+1;v<nb_col;v++)
				{
					if(Item[u] && Item[v])
						{
							if ( tabe[u]==1 && tabe[v]==1)	CurIndex[u][v]=Index[u][v];
							else if (tabe[u]==0 || tabe[v]==0) CurIndex[u][v]=0;
							else
								{
									x=taby[u][1];
									y=taby[v][1];
									max=Index[x][y];
									for (j=1;j<=tabe[u];j++)
										for (k=1;k<=tabe[v];k++)
											{
												double t=Index[taby[u][j]][taby[v][k]];  
												if (max<t) max=t;
												t=pow(max,tabe[u]);
												CurIndex[u][v]=pow(t,tabe[v]);
											}
								}
						}
				}
		max=1e-12;
		max_found=false;
		x=0;
		y=0;
		for (u=0;u<nb_col-1;u++)         //voir u=0 u<nb_col-1
			for (v=u+1;v<nb_col;v++)            //v=u+1;<nb_col
				{
					if(Item[u] && Item[v])
						{	
							if (tabe[u]==1 && tabe[v]==1) CurIndex[u][v]=Index[u][v];
							if (max<CurIndex[u][v])
								{
									max=CurIndex[u][v];
									x=u;
									y=v;
								}
						}
				}
    
		for (u=0;u<nb_col;u++) tabb[u]=-1;

		for (u=x;u<nb_col-1;u++)
			for (v=u+1;v<nb_col;v++)
				if (tabe[u]!=0 && tabe[v]!=0)
					if (CurIndex[u][v]==max)
						{
							tabb[u]=u;
							tabb[v]=v;
						}


		j=tabe[x];
		if(max>1e-12)
		{
			level[x]=f;

			for (u=x+1;u<nb_col;u++)
				{
					if(Item[u])
						{                                             
							if (tabb[u]!=-1 && CurIndex[x][u]==max && !max_found)
								{
									max_found=true;
									if(AlreadyInClasse[x]==-999999 && AlreadyInClasse[y]==-999999)
										{
											AlreadyInClasse[x]=f;
											AlreadyInClasse[y]=f;
											LevelX[f]=-x-1;
											LevelY[f]=-y-1;
										}
									else
										if(AlreadyInClasse[x]==-999999) 
											{
												LevelX[f]=-x-1;
												LevelY[f]=AlreadyInClasse[y];
												AlreadyInClasse[x]=f;
												AlreadyInClasse[y]=f;
											}
										else
											if(AlreadyInClasse[y]==-999999)
												{
													LevelX[f]=AlreadyInClasse[x];
													LevelY[f]=-y-1;
													AlreadyInClasse[x]=f;
													AlreadyInClasse[y]=f;
												}
											else
												{
													LevelX[f]=AlreadyInClasse[x];
													LevelY[f]=AlreadyInClasse[y];
													AlreadyInClasse[x]=f;
													AlreadyInClasse[y]=f;
												}
									Terminal[y]=0;
									GenericPair(x,y,tabe[x],tabe[y],GenPairX[f],GenPairY[f], Index, taby);


									level[u]=f;
									tabe[x]=tabe[x]+tabe[u];
									char * new_s = new char[strlen(cc[x])+2+strlen(cc[u])];
									strcpy(new_s,cc[x]);
									strcat(new_s," ");    //espace entre les 2 chaines
									strcat(new_s,cc[u]);
									delete []cc[u];
									cc[u]=0;
									delete []cc[x];
									cc[x]=new_s;
									char * new_s2 = new char[strlen(cl[x])+2+strlen(cl[u])];
									strcpy(new_s2,cl[x]);
									strcat(new_s2," ");    //espace entre les 2 chaines
									strcat(new_s2,cl[u]);
									delete []cl[u];
									cl[u]=NULL;
									delete []cl[x];
									cl[x]=new_s2;

									r--;
									for (k=j+1;k<=j+tabe[u];k++)
										taby[x][k]=taby[u][k-j];
									j=j+tabe[u];
									tabe[u]=0;
								}
						}
				}
				tabo[f]=(int)taby[x][1];
				tabz[f]=(int)taby[x][tabe[x]];
        
        Rprintf("Level %d tabo %d tabz %d\n",f,tabo[f],tabz[f]);
				tabee[f]=tabe[x];			
				
        char *new_s=new char[strlen(cc[x])+3];  //3 a cause des ()
				char *new_s2=new char[strlen(cl[x])+3];
				
        sprintf(new_s,"(%s)",cc[x]);         //on met la classe formée entre ()
				//attention wsprintf est limite a 1024 caractere
				strcpy(new_s,"(");
				strcat(new_s,cc[x]);    //espace entre les 2 chaines
				strcat(new_s,")");
				delete []cc[x];
				cc[x]=new_s;
				strcpy(new_s2,"(");
				strcat(new_s2,cl[x]);    //espace entre les 2 chaines
				strcat(new_s2,")");
				delete []cl[x];
				cl[x]=new_s2;
        Rprintf("Classification %d : %s similarity %f\n",f+1,cl[x],max);
        
        
				//os<<Classification<<(f+1)<<" : "<<cl[x]<<Similarity<<max<<"\r\n\r\n";
				f++;
	
        /*
				if(f%10==9)
					{
						os<<'\0';
						if(DisplayResult && !TextDesactivated) PView->GetEditCtrl().ReplaceSel(str);
						os.seekp(0);
					}
          */
			}

	
  }while(r>1 && max>1e-12);    
    
        
  j=0;
  k=0;
  for(i=0;i<nb_col;i++)
	{
		if(cc[i]) j+=strlen(cc[i])+1;
	}
	for(i=0;i<nb_col;i++)
	{
		if(cc[i]) k+=strlen(cl[i])+1;
	}
	
	char *chc=new char[j+1];
	chc[0]='\0';
	char *chl=new char[k+1];
	chl[0]='\0';
	for(i=0;i<nb_col;i++)
	{
		if(tabe[i])
		{
			strcat(chc,cc[i]);
			strcat(chc," ");
			strcat(chl,cl[i]);
			strcat(chl," ");
			
		}
	}


  
  SEXP results = PROTECT(allocVector(VECSXP, 4));
  SEXP listClasses = PROTECT(allocVector(VECSXP, 2));
  SET_VECTOR_ELT(listClasses, 0, mkString(chc));
  SET_VECTOR_ELT(listClasses, 1, mkString(chl));
  SET_VECTOR_ELT(results, 0, listClasses);
  
  SEXP Rtabo = PROTECT(allocVector(INTSXP, f));
  for(i=0;i<f;i++)
    INTEGER(Rtabo)[i]=tabo[i]+1;  //+1 because in R indexes start at 1
  SET_VECTOR_ELT(results, 1, Rtabo);
  
  SEXP Rtabz = PROTECT(allocVector(INTSXP, f));
  for(i=0;i<f;i++)
    INTEGER(Rtabz)[i]=tabz[i]+1;  //idem
  SET_VECTOR_ELT(results, 2, Rtabz);
  
  
  SEXP RnbLevel=PROTECT(allocVector(INTSXP, 1));
  INTEGER(RnbLevel)[0] = f;
  SET_VECTOR_ELT(results, 3, RnbLevel);
  
  UNPROTECT(5); 
  
  for(i;i<nb_col;i++)
    delete []Index[i];
  delete []Index;
  for(i;i<nb_col;i++)
    delete []CurIndex[i];
  delete []CurIndex;
  delete []AlreadyInClasse;
  delete []Terminal;
  delete []LevelX;
  delete []LevelY;
  for(i;i<nb_col;i++)
    delete []taby[i];
  for(i;i<nb_col;i++)
    if(cl[i]) delete []cl[i];
  delete []cl;
  for(i;i<nb_col;i++)
    if(cc[i]) delete []cc[i];
  delete []cc;
  delete []taby;
  delete []tabe;
  delete []GenPairX;
  delete []GenPairY;
  delete []tabee;
  delete []tabb;
  delete []tabz;
  delete []tabo;
  delete []Item;
  delete []level;
  return results;
  

}






}
