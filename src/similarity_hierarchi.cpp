#include <iostream>
#include <ostream>
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

using namespace std;

//This structure is used to sort the tuples used in the similarity and the hierarchy to compute the significant nodes
//Tuple are sorted by cohesion (or similarity), if both values are equal we compare the occurrences of x and then y
struct Local {
  Local(double *Occurrences) { this->Occurrences = Occurrences; }
  bool operator () (const tuple<double,int,int>& a, const tuple<double,int,int>& b) {
    double cohe_a=get<0>(a);
    double cohe_b=get<0>(b);
    int x_a=get<1>(a);
    int x_b=get<1>(b);
    int y_a=get<2>(a);
    int y_b=get<2>(b);
    
    
    return (cohe_a<cohe_b || 
    (cohe_a==cohe_b && (Occurrences[x_a]>Occurrences[x_b] ||
    (Occurrences[x_a]==Occurrences[x_b] && 
    Occurrences[y_a]>Occurrences[y_b]))));
  }
  
  double *Occurrences;
};



//This routines is used to create a vector without doublons
//If the element does not exist we add it into the vector
template <class T>
int
insertNoDuplicate( vector<T>& references, T const& newValue )
{
  int results = find( references.begin(), references.end(), newValue )
  - references.begin();
  if ( results == references.size() ) {
    references.push_back( newValue );
  }
  return results;
}




//functions from R should be in this part
extern "C"{
  
  
  //define a generic pair as page 31 in the book: Statistical implicative analysis theory and applications
  //a generic pair has the greater index among all the possible pairs in a class
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
  
  
  //this function computes significant levels for the hierarchy or similarity tree
  void SignificantLevel(double **indexes_values,int nb_col, double* occurrences_variables, int nb_levels, 
  int *variable_left,int *variable_right, int *size_class, int** classes_associated_with, int*  significant_nodes, bool verbose)
  {
    
    long ll=nb_col*(nb_col-1);
    double old_signi=0;
    
    //list of pairs of variables
    vector <tuple<double,int,int>> list_pairs;
    
    //list of pairs of pairs (all pairs possible at a given level)
    //in thie list we remove all the doublons
    vector <tuple<double,int,int>> list_pairs_pairs;   
    vector <tuple<double,int,int>>::iterator it;
    
    
    long i,j,k,l=0,m;
    
    //create all the possible pairs between all the variables
    //a tuple contains the value of the similarity or cohesion for 2 variables
    for(i=0;i<nb_col;i++)
    for(j=0;j<nb_col;j++)
    {  
      if(i!=j)
      {
        list_pairs.push_back(make_tuple(indexes_values[i][j],i,j));
      }
    }
    
    //pairs are sorted according the the index and the occurrences of variables in case of equality
    sort (list_pairs.begin(), list_pairs.end(), Local(occurrences_variables)); 
    
    
    //only to check the content of the list
    //cout << "list contains:"<<endl;
    //for (it=list_pairs.begin(); it!=list_pairs.end(); ++it) {
    //  cout << get<0>(*it) << ' '<<get<1>(*it) << ' '<<get<2>(*it) << ' '<<endl;
    //}
    
    double *signi=new double[nb_levels];
    double *localmax=new double[nb_levels];
    
    //for each level we need to compute the significance of this level
    for(i=0;i<nb_levels;i++)
    {
      if(verbose)
      cout<<endl<<" Level "<<i+1<<" : ";
      
      for(j=0;j<=i;j++)
      {
        int t;
        int u=variable_left[j];
        int v=variable_right[j];
        int p=size_class[j];
        //if the class has more than 2 elements
        //for all possible pairs in the class we add the pair (without doublons)
        if(p>=1)
        for (l=1;l<=p-1;l++)
        for (t=l+1;t<=p;t++)  																								  
        {
          int i=classes_associated_with[u][l];
          int j=classes_associated_with[u][t];
          insertNoDuplicate(list_pairs_pairs,make_tuple(indexes_values[i][j],i,j));
          
          //add it for double arrows in hierarchy
          //value>=10 means equivalent nodes
          if(significant_nodes[j]>=10) 
          insertNoDuplicate(list_pairs_pairs,make_tuple(indexes_values[j][i],j,i));
          
        }
        //if the class has only 2 elements
        if(p==0)
        {
          insertNoDuplicate(list_pairs_pairs,make_tuple(indexes_values[u][v],u,v));
        }
      }
      
      
      //the list is sorted by index
      sort (list_pairs_pairs.begin(), list_pairs_pairs.end(), Local(occurrences_variables));   
      //number of disctinc elements between all the possible pairs at this level
      int nb_elements_level=list_pairs_pairs.size();
      
      if(verbose) {
        cout<<"size list pairs "<<list_pairs.size()<<endl;
        for(l=0;l<nb_elements_level;l++)
        {
          if(l%11==10) cout<<endl;
          cout<<"c("<<(get<1>(list_pairs_pairs[l])+1)<<","<<(get<2>(list_pairs_pairs[l])+1)<<")="<<(get<0>(list_pairs_pairs[l]))<<" ";
        }
      }
      
      
      int *mark=new int[ll];
      for(l=0;l<ll;l++) mark[l]=0;
      int last=ll-1;
      int out;
      int nb=0;
      
      int val=nb_elements_level;
      
      //we start by the end because elements are in increasing order
      //l is used for the long list (with all possible pairs)
      for(l=nb_elements_level-1;l>=0;l--)
      {
        //last is used for the short list (only direct pairs)
        last=ll-1;
        
        out=0;
        while(last>=0 && !out)
        {
          // get<1> means element x
          // get<2> means element y
          //we compare x and y in both lists
          if(get<1>(list_pairs_pairs[l])==get<1>(list_pairs[last]) && get<2>(list_pairs_pairs[l])==get<2>(list_pairs[last]))
          {
            mark[last]=val--;
            out=1;
          }
          last--;
        }
        //Rprintf("last %d\n",last);
      }
      
      for(l=nb_elements_level-1;l>=0;l--)
      {
        k=ll-1;
        out=0;
        while(k>=0 && !out) {out=(mark[k--]==l+1);}
        int ind=k+1;
        
        // get<0> means current index value
        while(k>=0 && get<0>(list_pairs[ind])==get<0>(list_pairs[k]) 
        && occurrences_variables[get<1>(list_pairs[ind])]==occurrences_variables[get<1>(list_pairs[k])] 
        && occurrences_variables[get<2>(list_pairs[ind])]==occurrences_variables[get<2>(list_pairs[k])]) 
        {
          k--; 
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
      if(verbose)
      cout<<"    G-SR="<<nb<<"  S="<<signi[i]<<"  V="<<localmax[i];
      delete []mark;
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
    //Debug
    cout<<"Most significant level "<<index+1<<endl;
    
    for(i=0;i<nb_levels;i++)
    {
      int node=0;
      if(i==0 && nb_levels>0 && localmax[i]>localmax[i+1]) node=1;
      if(i>0 && i<nb_levels-1 && localmax[i]>=localmax[i-1] && localmax[i]>=localmax[i+1]) node=1;
      if(i==nb_levels-1 && i>0 && localmax[i]>localmax[i-1]) node=1;
      if(node) 
      {
        cout<<"Significant level "<<i+1<<endl; 
        significant_nodes[i]+=1;
      }
    }
  }
  
  
  
  SEXP similarity(SEXP similarity_matrix, SEXP list_occurrences_variables, SEXP Verbose) {
    if(!isMatrix(similarity_matrix))
    error("matrix needed");
    if(!isLogical(Verbose))
    error("verbose must be a boolean");
    
    bool verbose=LOGICAL(Verbose)[0];
    
    int nb_col=INTEGER(getAttrib(similarity_matrix, R_DimSymbol))[0];
    SEXP list_names=getAttrib(similarity_matrix, R_DimNamesSymbol);
    SEXP variables= VECTOR_ELT(list_names, 0);
    
    if(verbose)
    Rprintf("Nb col %d\n",nb_col);
    
    
    
    int i,j,k,u,v;
    int x,y;
    double max;
    bool max_found;
    
    int *level= new int[nb_col];
    
    double *Occurrences_variables = REAL(list_occurrences_variables);
    
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
      taby[i][1]=i;
      tabe[i]=1;
      int length=strlen(CHAR(STRING_ELT(variables, i)));
      cc[i]=new char[10];	//it is only to have the number of the variable
      cl[i]=new char[length+1];
      sprintf(cc[i],"%i",i+1);
      sprintf(cl[i],"%s",CHAR(STRING_ELT(variables, i)));
    }
    
    int r=nb_col;
    
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
        //Rprintf("%f ",Index[i][j]);   
      }
      //Rprintf("\n");
    }
    
    
    do
    {
      for (u=0;u<nb_col-1;u++)
      for (v=u+1;v<nb_col;v++) {
        if ( tabe[u]==1 && tabe[v]==1)	CurIndex[u][v]=Index[u][v];
        else if (tabe[u]==0 || tabe[v]==0) CurIndex[u][v]=0;
        else {
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
      max=1e-12;
      max_found=false;
      x=0;
      y=0;
      for (u=0;u<nb_col-1;u++)         //voir u=0 u<nb_col-1
      for (v=u+1;v<nb_col;v++)            //v=u+1;<nb_col
      {
        
        if (tabe[u]==1 && tabe[v]==1) CurIndex[u][v]=Index[u][v];
        if (max<CurIndex[u][v])
        {
          max=CurIndex[u][v];
          x=u;
          y=v;
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
        tabo[f]=(int)taby[x][1];
        tabz[f]=(int)taby[x][tabe[x]];
        if(verbose)
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
      //classes not selected are suppressed
      if(tabe[i] && AlreadyInClasse[i]>=0)
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
    SignificantLevel(Index, nb_col, Occurrences_variables,f,tabo,tabz,tabee,taby,significant_nodes,verbose);
    
    
    
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
    delete []level;
    return results;
    
    
  }
  
  
  double Produce(int u,int v,int p,int q, double** Index, int** taby)
  {
    double prod1=1,prod2=1,prod3=1;
    long l,t,nq;
    if(p>=1)
    for (l=1;l<=p-1;l++)
    for (t=l+1;t<=p;t++)																									  
    prod1=prod1*Index[taby[u][l]][taby[u][t]];
    if(q>=1)
    for (l=1;l<=q-1;l++)
    for (t=l+1;t<=q;t++)
    prod2=prod2*Index[taby[v][l]][taby[v][t]];
    for (l=1;l<=p;l++)
    for (t=1;t<=q;t++)
    prod3=prod3*Index[taby[u][l]][taby[v][t]];
    if (prod1==0 || prod2==0 || prod3==0) return 0;
    else
    {
      nq=p+q;
      nq=nq*(nq-1);
      return pow(prod1*prod2*prod3,2./nq);
    }
  }
  
  
  
  double Cohesion_classX(int q,int p, double** Index, int** taby)
  {
    int ordre;
    long l,t;
    double prod=1,d;
    if (p==1) ordre=0;
    else if (p==2) ordre=1;
    else if (p>2) ordre=2;
    switch (ordre)
    {
      case 0:d=1;break;
      case 1:
      d=Index[(int)taby[q][1]][(int)taby[q][p]];
      break;
      case 2:
      for (l=1;l<p;l++)
      for (t=l+1;t<=p;t++)
      prod=prod*Index[(int)taby[q][l]][(int)taby[q][t]];
      if (prod==0) d=0;
      else
      {
        d=pow(prod,2./(p*(p-1)));
      }
      break;
    }
    return d;
  }
  
  double ClassImpli(int l,int n,int p,int q, double c1, double c2, double** Index, int **taby)
  {
    int z;
    long i,j;
    double res=0,sup=0;
    
    z=p*q;
    if ((p==0)||(q==0)) res=0;
    else
    {
      for (i=1;i<=p;i++)
      for (j=1;j<=q;j++)
      if(sup<Index[(int)taby[l][i]][(int)taby[n][j]])
      sup=Index[(int)taby[l][i]][(int)taby[n][j]];
      res=pow(sup,p*q)*sqrt(c1*c2);
    }
    return res;
  }
  
  
  SEXP hierarchy(SEXP similarity_matrix, SEXP list_occurrences_variables, SEXP Verbose) {
    if(!isMatrix(similarity_matrix))
    error("matrix needed");
    
    int nb_col=INTEGER(getAttrib(similarity_matrix, R_DimSymbol))[0];
    SEXP list_names=getAttrib(similarity_matrix, R_DimNamesSymbol);
    SEXP variables= VECTOR_ELT(list_names, 0);
    
    if(!isLogical(Verbose))
    error("verbose must be a boolean");
    
    bool verbose=LOGICAL(Verbose)[0];
    
    if(verbose)
    Rprintf("Nb col %d\n",nb_col);
    
    
    
    int i,j,k,u,v;
    int x,y;
    double max=-1;
    bool max_found;
    
    int *level= new int[nb_col];
    double *Occurrences_variables = REAL(list_occurrences_variables);
    
    
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
      taby[i][1]=i;
      tabe[i]=1;
      int length=strlen(CHAR(STRING_ELT(variables, i)));
      cc[i]=new char[10];	//it is only to have the number of the variable
      cl[i]=new char[length+1];
      sprintf(cc[i],"%i",i+1);
      sprintf(cl[i],"%s",CHAR(STRING_ELT(variables, i)));
    }
    
    int r=nb_col;
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
    
    
    //put data into Index
    for(i=0;i<nb_col;i++) {
      for(j=0;j<nb_col;j++) {
        Index[i][j]=val_mat[j*nb_col+i];  //element in the matrix seems to be transposed
        //Rprintf("%f ",Index[i][j]);   
      }
      //Rprintf("\n");
    }
    
    
    while(r>1 && max!=0)
    {
      for (u=0;u<nb_col;u++)
      for (v=0;v<nb_col;v++)
      {
        
        if ( tabe[u]==1 && tabe[v]==1)	CurIndex[u][v]=Index[u][v];
        else if (tabe[u]==0 || tabe[v]==0 || u==v) CurIndex[u][v]=0;
        else
        CurIndex[u][v]=Produce(u,v,tabe[u],tabe[v], Index, taby);
        
        
      }
      max=-1;
      
      x=0;
      y=0;
      for (u=0;u<nb_col;u++)        
      for (v=0;v<nb_col;v++)      
      {
          if (u!=v && max<=CurIndex[u][v])
          {
            if (max>0 && max==CurIndex[u][v])//ordre de pref cohe de la nouvelle class,
            {												//impli de la nouvelle classe, cohe interne
            //calcul de phi(A,B) et phi(B,A)
            double c1=Cohesion_classX(x,tabe[x],Index,taby);
            double c2=Cohesion_classX(u,tabe[u],Index,taby);
            double impl1=ClassImpli(x,y,u,v,c1,c2,Index,taby); //impl de la class max
            double impl2=ClassImpli(u,v,x,y,c2,c1,Index,taby);
            if(impl1==impl2)
            {
              if(c1<c2)
              {
                x=u;	//on garde la classe qui a la plus forte cohe interne
                y=v;
              }
              else
              if(c1==c2)
              {
                //signale le pb a regis (avec exemple 41)
                if(x==v && y==u)
                significant_nodes[f]=10;
              }
            }
            else
            if(impl1<impl2)
            {
              x=u;	//on garde la classe qui a le plus fort phi
              y=v;
            }
            
            }
            else
            {
              max=CurIndex[u][v];
              x=u;
              y=v;
              significant_nodes[f]=0;
            }
          }
        
      }
      
      for (u=0;u<nb_col;u++) tabb[u]=-1;
      tabb[x]=x;tabb[y]=y;
      
      j=tabe[x];
      
      u=y;
      
      {                                             //a regarder bizare
      if ((tabb[u]!=-1)&&(CurIndex[x][u]==max)&&(CurIndex[x][u]!=0))
      {
        
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
        GenericPair(x,y,tabe[x],tabe[y],GenPairX[f],GenPairY[f],Index,taby);
        tabe[x]=tabe[x]+tabe[u];
        char * new_s = new char[strlen(cc[x])+2+strlen(cc[u])];
        strcpy(new_s,cc[x]);
        strcat(new_s," ");    //espace entre les 2 chaines
        strcat(new_s,cc[u]);
        delete []cc[u];
        cc[u]=NULL;
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
        
        r=r-1;
        for (k=j+1;k<=j+tabe[u];k++)
        taby[x][k]=taby[u][k-j];
        j=j+tabe[u];
        tabe[u]=0;
      }
      }
      if (max!=0)
      {
        tabo[f]=taby[x][1];
        tabz[f]=taby[x][tabe[x]];
        tabee[f]=tabe[x];
        char *new_s=new char[strlen(cc[x])+3];  //3 a cause des ()
        char *new_s2=new char[strlen(cl[x])+3];
        //wsprintf(new_s,"(%s)",cc[x]);         //on met la classe formée entre ()
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
        double a=Cohesion_classX(x,tabe[x],Index,taby);
        Rprintf("Classification %d : %s  Cohesion %f\n",(f+1),cl[x],a);
        //tab_cohe[f]=a;
        if (max) f++;
        
      }
      
      
      
      
      
    }
    
    
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
    //tabe=list_finel_nodes
    SignificantLevel(Index, nb_col, Occurrences_variables,f,tabo,tabz,tabee,taby,significant_nodes,verbose);
    
    
    SEXP results = PROTECT(allocVector(VECSXP, 6));
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
    
    SEXP Rfinal_nodes=PROTECT(allocVector(INTSXP, nb_col));
    for(i=0;i<nb_col;i++)
    INTEGER(Rfinal_nodes)[i]=tabe[i];
    SET_VECTOR_ELT(results, 5, Rfinal_nodes);
    
    
    UNPROTECT(7); 
    
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
  
  
  //function to build the dynamic cloud to partition the data
  SEXP dynamic_cloud(SEXP vector, SEXP number_partition) {
    
    if(!isVector(vector))
    error("vector needed");
    if(!isInteger(number_partition))
    error("integer needed");
    
    int nb_elt=length(vector);
    double *v_data=new double[nb_elt];
    for (int i=0;i<nb_elt;i++){
      v_data[i]=REAL(vector)[i];
    }
    double *copy_data=new double[nb_elt];
    for (int i=0;i<nb_elt;i++){
      copy_data[i]=v_data[i];
    }
    int nb_partition=INTEGER(number_partition)[0];
    cout<<nb_partition<<endl;
    
    cout<<nb_elt<<endl;
    int i,j,k;
    
    std::vector<double> myvector (v_data, v_data+nb_elt);
    sort (myvector.begin(), myvector.end());
    
    i=0;
    for (std::vector<double>::iterator it=myvector.begin(); it!=myvector.end(); ++it){
      
      v_data[i++]=*it;
    }
    
    int start[nb_partition];
    int end[nb_partition];
    double val_start[nb_partition];
    double val_end[nb_partition];
    double W,old_W; 
    double g[nb_partition];
    int index,old_index;
    
    double min,exp;
    
    start[0]=0;
    end[nb_partition-1]=nb_elt-1;
    for(j=0;j<nb_partition-1;j++)
    {
      end[j]=((j+1)*nb_elt)/(nb_partition)-1;
      start[j+1]=((j+1)*nb_elt)/(nb_partition);
    }
    old_W=-1;
    W=0;
    while(old_W!=W)
    {
      old_W=W;
      W=0;
      for(j=0;j<nb_partition;j++)
      {
        g[j]=0;
        for(k=start[j];k<=end[j];k++) {
          g[j]+=v_data[k];
        }
        g[j]/=(end[j]-start[j]+1);
      }
      old_index=0;
      for(k=0;k<nb_elt;k++)
      {
        min=1E300;
        index=-1;
        
        for(j=0;j<nb_partition;j++)
        {
          exp=pow(g[j]-v_data[k],2);
          
          if(min>exp)
          {
            min=exp;
            index=j;
          }
        }
        W+=min;
        if(index!=old_index)
        {
          end[old_index]=k-1;
          start[index]=k;
          old_index=index;
        }
      }
      cout<<"W "<<W<<endl;
    }
    
    cout<<endl<<"Optimal Parameters"<<endl;
    for(j=0;j<nb_partition;j++)
    {
      val_start[j]=v_data[start[j]];
      val_end[j]=v_data[end[j]];
      cout<<"From "<<val_start[j]<<" To "<<val_end[j]<<endl;
    }
    
    
    SEXP result = PROTECT(allocVector(INTSXP, nb_elt));
    for(i=0;i<nb_elt;i++) {
      int v=0;
      for(k=0;k<nb_partition;k++)
      if(copy_data[i]>=val_start[k] && copy_data[i]<=val_end[k])
      v=k+1;
      INTEGER(result)[i]=v;
    }
    //SEXP result = PROTECT(allocVector(REALSXP, 1));
    //REAL(result)[0] = 12+12;
    UNPROTECT(1);
    
    return result;
    
    delete []v_data;
    delete []copy_data;
    
  }    
  
  
  //function to build the file transaction.tab for apriori
  SEXP write_transactions(SEXP data) {
    if(!isMatrix(data))
    error("matrix needed");
    
    SEXP list_names=getAttrib(data, R_DimNamesSymbol);
    SEXP name= VECTOR_ELT(list_names, 1);
    
    int nb_col=INTEGER(getAttrib(data, R_DimSymbol))[1];
    int nb_row=INTEGER(getAttrib(data, R_DimSymbol))[0];
    
    double *val_mat=REAL(data);
    char **var=new char*[nb_col];
    for(int j=0;j<nb_col;j++) {
      var[j]=new char[strlen(CHAR(STRING_ELT(name, j)))+5];
      strcpy(var[j],CHAR(STRING_ELT(name, j)));
    }
    
    
    
    ofstream file;
    file.open ("transaction.tab");
    for(int i=0;i<nb_row;i++) {
      for(int j=0;j<nb_col;j++) {
        //warning inverse order...
        double v=val_mat[j*nb_row+i];
        if(v!=0)
        file<<var[j]<<" "<<v<<" ";
      }
      file<<endl;
    }
    file.close();
    
    for(int j=0;j<nb_col;j++) {
      delete []var[j];
    }
    delete []var;
    
    return(R_NilValue);
  }
  
  
  
}