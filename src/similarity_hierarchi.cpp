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



// list::sort
#include <vector>
#include <list>
#include <string>
#include <cctype>
#include <algorithm> 
#include <array> 



struct Local {
    Local(double *Occurrences) { this->Occurrences = Occurrences; }
    bool operator () (const std::tuple<double,int,int>& a, const std::tuple<double,int,int>& b) {
      double cohe_a=std::get<0>(a);
      double cohe_b=std::get<0>(b);
      int x_a=std::get<1>(a);
      int x_b=std::get<1>(b);
      int y_a=std::get<2>(a);
      int y_b=std::get<2>(b);
  
      
      return (cohe_a<cohe_b || 
          (cohe_a==cohe_b && (Occurrences[x_a]>Occurrences[x_b] ||
          (Occurrences[x_a]==Occurrences[x_b] && 
          Occurrences[y_a]>Occurrences[y_b]))));
    }

    double *Occurrences;
};




template <class T>
int
insertNoDuplicate( std::vector<T>& references, T const& newValue )
{
    int results = std::find( references.begin(), references.end(), newValue )
                                    - references.begin();
    if ( results == references.size() ) {
        references.push_back( newValue );
    }
    return results;
}

extern "C"{









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



void SignificantLevel(double **indexes_values,int nb_col, double* occurrences_variables, int nb_levels, 
int *variable_left,int *variable_right, int *size_class, int** classes_associated_with, int*  significant_nodes)
{
  //double* cohe;
  //int *order;
  int *x,*y;
  long ll=nb_col*(nb_col-1);
  double old_signi=0;
  //cohe = new double[ll];
  
  std::vector <std::tuple<double,int,int>> list_pairs;
  //list of pairs of pairs (all pairs possible at a given level)
  std::vector <std::tuple<double,int,int>> list_pairs_pairs;   
  std::vector <std::tuple<double,int,int>>::iterator it;
  
  
  long i,j,k,l=0,m;
  
  
  for(i=0;i<nb_col;i++)
    for(j=0;j<nb_col;j++)
    {  
      if(i!=j)
      {
        list_pairs.push_back(std::make_tuple(indexes_values[i][j],i,j));
//        cohe[l]=Index[i][j];
//        x[l]=i;
//        y[l]=j;
        
      }
    }
  
  std::sort (list_pairs.begin(), list_pairs.end(), Local(occurrences_variables)); 
  
  std::cout << "list contains:"<<std::endl;
  for (it=list_pairs.begin(); it!=list_pairs.end(); ++it) {
    std::cout << std::get<0>(*it) << ' '<<std::get<1>(*it) << ' '<<std::get<2>(*it) << ' '<<std::endl;
  }
  
  
//  char * str=new char[ll*50+20000]; 
//  ostrstream os(str,ll*50+20000);
//  os.precision(3);
//  
//  
//  CString Text;
  double *signi=new double[nb_levels];
  double *localmax=new double[nb_levels];
  
  for(i=0;i<nb_levels;i++)
  {
//    if(LongCalculation)
//    {
//      CString level;
//      level.LoadString(IDS_LEVEL);
      std::cout<<std::endl<<" Level "<<i+1<<" : ";
//    }
    for(j=0;j<=i;j++)
    {
      int t;
      int u=variable_left[j];
      int v=variable_right[j];
      int p=size_class[j];
//      int u=tabo[j];
//      int v=tabz[j];
//      int p=tabee[j];
      if(p>=1)
        for (l=1;l<=p-1;l++)
          for (t=l+1;t<=p;t++)  																								  
          {
            int i=classes_associated_with[u][l];
            int j=classes_associated_with[u][t];
            insertNoDuplicate(list_pairs_pairs,std::make_tuple(indexes_values[i][j],i,j));
            
            //list.Insert((int)taby[u][l],(int)taby[u][t]); 
            
            //add it for double arrows in hierarchy
            //value>=10 means equivalent nodes
            if(significant_nodes[j]>=10) 
              insertNoDuplicate(list_pairs_pairs,std::make_tuple(indexes_values[j][i],j,i));
            
          }
      if(p==0)
      {
        //list.Insert(u,v); 
        insertNoDuplicate(list_pairs_pairs,std::make_tuple(indexes_values[u][v],u,v));
      }
    }
//    int nb_ele_niv=list.GetCount();
//    int *uni_x=new int[nb_ele_niv];
//    int *uni_y=new int[nb_ele_niv];
//    double *uni_cohe = new double[nb_ele_niv];
//    
//    l=0;
//    POSITION pos=list.GetHeadPosition();
//    while(list.GetCount()>l)
//    {
//      int*mm = list.GetNext(pos);
//      
//      uni_x[l]=mm[0];
//      uni_y[l]=mm[1];
//      uni_cohe[l]=Index[mm[0]][mm[1]];
//      l++;
//      delete []mm; //indispensable pour recuperer la mem
//    }
//    list.RemoveAll();
    
    
    
    //qs(uni_cohe,uni_x,uni_y,nb_ele_niv);
    std::sort (list_pairs_pairs.begin(), list_pairs_pairs.end(), Local(occurrences_variables));   
    int nb_elements_level=list_pairs_pairs.size();
//    
//    
//    if(LongCalculation)
//    {
        std::cout<<"size list pairs "<<list_pairs.size()<<std::endl;
        for(l=0;l<nb_elements_level;l++)
        {
          if(l%11==10) std::cout<<std::endl;
          std::cout<<"c("<<(std::get<1>(list_pairs_pairs[l])+1)<<","<<(std::get<2>(list_pairs_pairs[l])+1)<<")="<<(std::get<0>(list_pairs_pairs[l]))<<" ";
        }
//    }
    int *mark=new int[ll];
    for(l=0;l<ll;l++) mark[l]=0;
    int last=ll-1;
    int out;
    int nb=0;
    
    int val=nb_elements_level;
    
    for(l=nb_elements_level-1;l>=0;l--)
    {
      last=ll-1;//cette instruction n'est pas indispensable en theorie
      //mais si on l'enleve il y a des pb a cause des quicksorts qui peuvent
      //trier des valeurs egales dans des ordes diff 
      out=0;
      while(last>=0 && !out)
      {
        // get<1> means element x
        // get<2> means element y
        if(std::get<1>(list_pairs_pairs[l])==std::get<1>(list_pairs[last]) && std::get<2>(list_pairs_pairs[l])==std::get<2>(list_pairs[last]))
        {
          mark[last]=val--;
          out=1;
        }
        last--;
      }
    }
//    //if(val!=0) MessageBeep(0);
    for(l=nb_elements_level-1;l>=0;l--)
    {
      k=ll-1;
      out=0;
      while(k>=0 && !out) {out=(mark[k--]==l+1);}
      int ind=k+1;
      
      // get<0> means current index value
      while(k>=0 && std::get<0>(list_pairs[ind])==std::get<0>(list_pairs[k]) 
              && occurrences_variables[std::get<1>(list_pairs[ind])]==occurrences_variables[std::get<1>(list_pairs[k])] 
              && occurrences_variables[std::get<2>(list_pairs[ind])]==occurrences_variables[std::get<2>(list_pairs[k])]) 
      {
        k--; 
        //os<<"ah "; //message pour vérifier ou interviennent les cas ou les cohe sont =
      }
      for(m=k;m>=0;m--)
      {
        if(!mark[m]) nb++;
      }
    }
    double sr=nb_elements_level*(ll-nb_elements_level);
    
    signi[i]=(nb-0.5*sr)/(sqrt(sr*(ll+1)/12.));
    if(i!=0) localmax[i]=signi[i]-signi[i-1];
    else localmax[i]=signi[0];
//    if(LongCalculation)
//      os<<"    G-SR="<<nb<<"  S="<<signi[i]<<"  V="<<localmax[i];

    std::cout<<"    G-SR="<<nb<<"  S="<<signi[i]<<"  V="<<localmax[i];
    delete []mark;
//    delete []uni_x;
//    delete []uni_y;
//    delete []uni_cohe;
  }
  double max=0;
  int index=0;
  for(i=0;i<nb_levels;i++)
  {
    if(max<localmax[i]) 
    {
      max=localmax[i];
      index=i;
    }
  }
//  Text.LoadString(IDS_MOSTSIGNIFICANT);
  std::cout<<"Most significant level "<<index+1<<std::endl;
//  
//  Text.LoadString(IDS_SIGNIFICANTNODES);
//  os<<"\r\n\r\n"<<Text<<"\r\n";
//  Text.LoadString(IDS_ATLEVEL);
  for(i=0;i<nb_levels;i++)
  {
    int node=0;
    if(i==0 && nb_levels>0 && localmax[i]>localmax[i+1]) node=1;
    if(i>0 && i<nb_levels-1 && localmax[i]>=localmax[i-1] && localmax[i]>=localmax[i+1]) node=1;
    if(i==nb_levels-1 && i>0 && localmax[i]>localmax[i-1]) node=1;
    if(node) 
    {
      std::cout<<"Significant level "<<i+1<<std::endl; 
      significant_nodes[i]+=1;
    }
  }
//  os<<"\r\n"<<'\0';
//  if(DisplayResult && !TextDesactivated) PView->GetEditCtrl().ReplaceSel(str);
//  delete []x;
//  delete []y;
//  delete []str;
//  delete []cohe;
//  delete []signi;
//  delete []localmax;
//  //	delete []order;
  
  
}



SEXP similarity(SEXP similarity_matrix,SEXP list_selected_items, SEXP list_occurrences_variables) {
  if(!isMatrix(similarity_matrix))
    error("matrix needed");
  
  int nb_col=INTEGER(getAttrib(similarity_matrix, R_DimSymbol))[0];
  SEXP list_names=getAttrib(similarity_matrix, R_DimNamesSymbol);
  SEXP variables= VECTOR_ELT(list_names, 0);
  
  if(!isVector(list_selected_items))
    error("vector needed");
  //if(INTEGER(getAttrib(list_selected_items, R_DimSymbol))[0]!=nb_col)  
  //  error("list of selected items should be the same size than the number of elements");
  Rprintf("Nb col %d\n",nb_col);
  
  
  
  int i,j,k,u,v;
  int x,y;
  double max;
  bool max_found;
  
  int *level= new int[nb_col];
  //int *Item=new int[nb_col];
  int *Item = INTEGER(list_selected_items);
  double *Occurrences_variables = REAL(list_occurrences_variables);
  /*for(i=0;i<nb_col;i++) {
    Rprintf("%d\n",Item[i]);
  }*/
  
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
  int *significant_nodes=new int[nb_col];
	for(i=0;i<nb_col;i++) 
	{
		cc[i]=0;
		cl[i]=0;
    significant_nodes[i]=0;
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


//tabo=variable_left
//tabz=variable_right
//tabee=size_class
//taby=classes_associated_with
  SignificantLevel(Index, nb_col, Occurrences_variables,f,tabo,tabz,tabee,taby,significant_nodes);


  
  SEXP results = PROTECT(allocVector(VECSXP, 5));
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
  
  SEXP Rsignificant_nodes=PROTECT(allocVector(INTSXP, nb_col));
  for(i=0;i<nb_col;i++)
    INTEGER(Rsignificant_nodes)[i]=significant_nodes[i];
  
  SET_VECTOR_ELT(results, 4, Rsignificant_nodes);
  
  
  UNPROTECT(6); 
  
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
  delete []significant_nodes;
  //delete []Item;
  delete []level;
  return results;
  

}











}
