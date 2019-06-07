#include <Rcpp.h>
#include <iostream>
#include <ostream>
#include <math.h>


#include <sstream>
#include <fstream>
#include <string>

#include <cstring>
#include <math.h>
#include <R.h>
#include <Rinternals.h>



// list::sort
#include <vector>
#include <list>
#include <cctype>

#include <array> 



using namespace std;
using namespace Rcpp;

//template <typename T>
//using TwoDdouble = vector<vector<T>>;



typedef vector < vector< double > > TwoDdouble;
typedef vector < vector< int > > TwoDint;


//This structure is used to sort the tuples used in the similarity and the hierarchy to compute the significant nodes
//Tuple are sorted by cohesion (or similarity), if both values are equal we compare the occurrences of x and then y
struct Local {
  Local( vector<double>& Occurrences2):Occurrences(Occurrences2) {  }
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
  
  vector<double>& Occurrences;
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





struct ordering {
  bool operator ()(pair<double, int> const& a, pair<double, int> const& b) {
    return (a.first) < (b.first);
  }
};




void ItemInConstitution(vector<int>& level,int& index,int v, vector<int>& LevelX, vector<int>& LevelY)
{
  level[index++]=v;
  if(LevelX[v]>=0)
  {
    ItemInConstitution(level,index,LevelX[v],LevelX,LevelY);
  }
  
  if(LevelY[v]>=0)
  {
    ItemInConstitution(level,index,LevelY[v],LevelX,LevelY);
  }
  
}

/*
void LevelInConstitution(vector<int>& level,int& index,int v, vector<int>& LevelX, vector<int>& LevelY)
{
  level[index++]=v;
  cout<<"level "<<level[index]<<endl;
  if(LevelX[v]>=0)
  {
    LevelInConstitution(level,index,LevelX[v], LevelX, LevelY);
  }
  if(LevelY[v]>=0)
  {
    LevelInConstitution(level,index,LevelY[v], LevelX, LevelY);
  }
}
*/


//defined in istree.c
long double Cnp(int n,int p) ;

float Normal(double val);




double FormImpli(double a,double b)
{
  if(b>=a) return 0.5+a/2+log((b-a)/5+1);
  else return 0.5-a/2+b*exp(b-a);
}



















//function to build the file transaction.tab for apriori
// [[Rcpp::export]]
void write_transactions(NumericMatrix data) {
  
  
  
  List l=data.attr("dimnames");
  List l2=l[1];
  int nb_col=data.ncol();
  int nb_row=data.nrow();
  
  
  ofstream file;
  file.open ("transaction.tab");
  for(int i=0;i<nb_row;i++) {
    for(int j=0;j<nb_col;j++) {
      
      double v=data(i,j);
      string name=l2[j];
      if(v!=0)
      file<<name<<" "<<v<<" ";
    }
    file<<endl;
  }
  file.close();
  
}





//function to build the dynamic cloud to partition the data
// [[Rcpp::export]]
IntegerVector dynamic_cloud(NumericVector data, IntegerVector number_partition) {
  
  if(number_partition.size()>1)
  throw std::range_error("number of partition must be a scalar");
  
  int nb_elt=data.size();
  
  int nb_partition=number_partition[0];
  cout<<nb_partition<<endl;
  
  cout<<nb_elt<<endl;
  int i,j,k;
  
  //vector<double> myvector (v_data, v_data+nb_elt);
  
  
  vector<double> copy_data(data.size());
  
  for (int i=0;i<data.size();i++){
    copy_data[i]=data[i];
  }
  
  sort (data.begin(), data.end());
  
  i=0;
  /*for (NumericVector::iterator it=data.begin(); it!=data.end(); ++it){
    cout<<*it<<endl;
    //v_data[i++]=*it;
  }*/
  
  
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
        g[j]+=data[k];
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
        exp=pow(g[j]-data[k],2);
        
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
    //cout<<"W "<<W<<endl;
  }
  
  cout<<endl<<"Optimal Parameters"<<endl;
  for(j=0;j<nb_partition;j++)
  {
    val_start[j]=data[start[j]];
    val_end[j]=data[end[j]];
    cout<<"From "<<val_start[j]<<" To "<<val_end[j]<<endl;
  }
  
  
  IntegerVector result(nb_elt);
  for(i=0;i<nb_elt;i++) {
    int v=0;
    for(k=0;k<nb_partition;k++)
    if(copy_data[i]>=val_start[k] && copy_data[i]<=val_end[k])
    v=k+1;
    result[i]=v;
  }
  
  return result;
  
} 








//define a generic pair as page 31 in the book: Statistical implicative analysis theory and applications
//a generic pair has the greater index among all the possible pairs in a class
void GenericPair(int u,int v,int p,int q,int &maxx, int& maxy, TwoDdouble & Index, TwoDint& classes_associated_with)
{
  int l,t;
  double max=-1;
  double ind;
  for (l=1;l<=p;l++)
  for (t=1;t<=q;t++)
  {
    ind=Index[classes_associated_with[u][l]][classes_associated_with[v][t]];
    if(ind>max) 
    {
      maxx=classes_associated_with[u][l];
      maxy=classes_associated_with[v][t];
      max=ind;
    }
  }
}





double Produce(int u,int v,int p,int q, TwoDdouble& Index, TwoDint& classes_associated_with)
{
  double prod1=1,prod2=1,prod3=1;
  long l,t,nq;
  if(p>=1)
  for (l=1;l<=p-1;l++)
  for (t=l+1;t<=p;t++)  																								  
  prod1=prod1*Index[classes_associated_with[u][l]][classes_associated_with[u][t]];
  if(q>=1)
  for (l=1;l<=q-1;l++)
  for (t=l+1;t<=q;t++)
  prod2=prod2*Index[classes_associated_with[v][l]][classes_associated_with[v][t]];
  for (l=1;l<=p;l++)
  for (t=1;t<=q;t++)
  prod3=prod3*Index[classes_associated_with[u][l]][classes_associated_with[v][t]];
  if (prod1==0 || prod2==0 || prod3==0) return 0;
  else
  {
    nq=p+q;
    nq=nq*(nq-1);
    return pow(prod1*prod2*prod3,2./nq);
  }
}



double Cohesion_classX(int q,int p, TwoDdouble& Index, TwoDint& classes_associated_with)
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
    d=Index[(int)classes_associated_with[q][1]][(int)classes_associated_with[q][p]];
    break;
    case 2:
    for (l=1;l<p;l++)
    for (t=l+1;t<=p;t++)
    prod=prod*Index[(int)classes_associated_with[q][l]][(int)classes_associated_with[q][t]];
    if (prod==0) d=0;
    else
    {
      d=pow(prod,2./(p*(p-1)));
    }
    break;
  }
  return d;
}

double ClassImpli(int l,int n,int p,int q, double c1, double c2, TwoDdouble& Index, TwoDint& classes_associated_with)
{
  int z;
  long i,j;
  double res=0,sup=0;
  
  z=p*q;
  if ((p==0)||(q==0)) res=0;
  else
  {
    for (i=1;i<=p;i++){
      for (j=1;j<=q;j++){
        if(sup<Index[classes_associated_with[l][i]][classes_associated_with[n][j]]) {
          sup=Index[classes_associated_with[l][i]][classes_associated_with[n][j]];
        }
      }
    }
    res=pow(sup,p*q)*sqrt(c1*c2);
  }
  return res;
}



//this function computes significant levels for the hierarchy or similarity tree
void SignificantLevel(TwoDdouble& indexes_values,int nb_col, vector<double>& occurrences_variables, int nb_levels, 
vector<int>& variable_left,vector<int>& variable_right, vector<int>& size_class, TwoDint& classes_associated_with,
vector<int>&  significant_nodes,
bool verbose)
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
  
  vector<double> signi(nb_levels);
  vector<double> localmax(nb_levels);
  
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
    
    
    vector<int> mark(ll);
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
  if(verbose) cout<<"Most significant level "<<index+1<<endl;
  
  for(i=0;i<nb_levels;i++)
  {
    int node=0;
    if(i==0 && nb_levels>0 && localmax[i]>localmax[i+1]) node=1;
    if(i>0 && i<nb_levels-1 && localmax[i]>=localmax[i-1] && localmax[i]>=localmax[i+1]) node=1;
    if(i==nb_levels-1 && i>0 && localmax[i]>localmax[i-1]) node=1;
    if(node) 
    {
      if(verbose) cout<<"Significant level "<<i+1<<endl; 
      significant_nodes[i]+=1;
    }
  }
}




void contributiveCategories(NumericMatrix supplementary_variables,TwoDdouble &index_simi,int nb_levels,
vector<int>& GenPairX, vector<int>& GenPairY, vector<int>& LevelX, vector<int>& LevelY, TwoDdouble& matrix_values, 
int Typi, int nb_col, int nb_row, List individuals, vector<string>& string_level, bool hierarchy, bool verbose)
{
  
  
  int i,j;
  
  //nb of supplementary variables
  
  List list_names=supplementary_variables.attr("dimnames");
  List supplementary_variable=list_names[1];
  
  int nb_comp_var=supplementary_variable.size();
  
  vector<string>   supp_var(nb_comp_var);
  
  for(int i=0;i<nb_comp_var;i++) {
    string v=supplementary_variable[i];
    string v2(v,0,strlen(v.c_str())-2);
    supp_var[i]=v2;
  }
  
  
  TwoDdouble supplementary_values(nb_row,vector<double> (nb_comp_var));
  
  for(int i=0;i<nb_row;i++)
  for(int j=0;j<nb_comp_var;j++) {
    supplementary_values[i][j]=supplementary_variables(i,j);
  }
  
  
  
  int k,nb,l;
  for(nb=0;nb<nb_levels;nb++)
  {
    
    
    vector<int> level(nb_col);
    vector<double> ImpliVector(nb_col);
    vector<double> Contrib(nb_row);
    vector<double> Contrib_copy(nb_row);
    vector<int> Contrib_index(nb_row);
    
    
    
    int nb_sub_level=0;
    //compute the sub-levels involved
    //LevelInConstitution
    
    ItemInConstitution(level,nb_sub_level,nb, LevelX, LevelY);
    //to see later
    //sort(level.begin(),level.begin()+nb_sub_level);   
    
    
    
    
    i=0;
    
    
    
    
    
    if(Typi)
    cout<<"Typicality to the sublevels: ";
    else
    cout<<"Contribution to the sublevels: ";
    cout<<string_level[nb]<<" with classes at levels ";
    for(int i=0;i<nb_sub_level;i++) {
      cout<<level[i]+1<<" ";
    }
    cout<<endl;
    
    for(int i=0;i<nb_sub_level;i++) {
      ImpliVector[level[i]]=index_simi[GenPairX[i]][GenPairY[i]];
    }
    

    if(Typi)
    {
      for(i=0;i<nb_row;i++)
      {
        double cont=0;
        for(j=0;j<nb_sub_level;j++)
        {
          double a=matrix_values[i][GenPairX[level[j]]];
          double b=matrix_values[i][GenPairY[level[j]]];
          double phi;
          if(hierarchy) {
            phi=FormImpli(a,b); 
          }
          else {
            phi=a*b;
          }
          if (ImpliVector[level[j]]==1) 
          ImpliVector[level[j]]=0.999999999999;
          cont+=pow(ImpliVector[level[j]]-phi,2)/(1-ImpliVector[level[j]]);
        }
        cont=pow(1./(double)nb_sub_level*cont,0.5);
        Contrib[i]=cont;
      
      }
      double max=0;
      for(i=0;i<nb_row;i++) {
        if(max<Contrib[i])
        max=Contrib[i];
      }
      for(i=0;i<nb_row;i++) {
        Contrib[i]=(max-Contrib[i])/max;
      }
    }
    else
    {
      for(i=0;i<nb_row;i++)
      {
        l=0;
        double Cont=0;
        for(j=0;j<nb_sub_level;j++)
        {
          double a=matrix_values[i][GenPairX[level[j]]];
          double b=matrix_values[i][GenPairY[level[j]]];
          double phi;
          if(hierarchy) {
            phi=FormImpli(a,b); 
          }
          else {
            phi=a*b;
          }
          Cont+=pow(1-phi,2);
          l++;
        }
        
        Contrib[i]=1-sqrt(1./(double)nb_sub_level*Cont);
      }
    }
    
    
    
    
    typedef vector<pair<double, int>>::const_iterator myiter;
    vector<pair<double, int> > order(nb_row);
    
    
    
    for(i=0;i<nb_row;i++) 
    {
      order[i]=make_pair(Contrib[i],i);
    }
    
    //sort the elements, we need the ordering of the individuals with their contribution
    sort(order.begin(), order.end(), ordering());
    
    i=0;
    for (myiter it=order.begin(); it!=order.end(); ++it) {
      //cout << get<0>(*it) << ' '<<get<1>(*it) << ' '<<endl;
      Contrib_copy[i]=get<0>(*it);
      Contrib_index[i]=get<1>(*it);
      i++;
    }
    
    
    
    double GroupValue = Contrib_copy[nb_row-1];
    double MiddleContrib=0;
    for(i=0;i<nb_row;i++) MiddleContrib+=Contrib[i];
    MiddleContrib=MiddleContrib/(double)nb_row;
    
    i=1;
    double NewExplainedVariance;
    double ExplainedVariance=i/(double)(nb_row-i)*(GroupValue-MiddleContrib)*(GroupValue-MiddleContrib);
    int OptimalGroup=nb_row-1;
    
    i=2;
    GroupValue=GroupValue+1/(double)i*(Contrib_copy[nb_row-i]-GroupValue);
    NewExplainedVariance=i/(double)(nb_row-i)*(GroupValue-MiddleContrib)*(GroupValue-MiddleContrib);
    while(NewExplainedVariance>=ExplainedVariance)
    {
      OptimalGroup--;
      ExplainedVariance=NewExplainedVariance;
      i+=1;
      GroupValue=GroupValue+1/(double)i*(Contrib_copy[nb_row-i]-GroupValue);
      NewExplainedVariance=i/(double)(nb_row-i)*(GroupValue-MiddleContrib)*(GroupValue-MiddleContrib);
    }
    if(verbose)
    {
      
      cout<<"Optimal group"<<endl;
      for(i=OptimalGroup;i<nb_row;i++) 
      {
        if((i-OptimalGroup-1)%11==10) cout<<endl;
        string v=individuals[Contrib_index[i]];
        cout<<v<<" ";
        
        
      }
      cout<<endl;
    }
    
    
    double inter;
    int n0=nb_row-OptimalGroup;
    double p=(double)n0/(double)nb_row;
    double un_p=1.-p;
    double proba;
    double min=999;
    int index=0;
    double occ;
    
    int l;
    if (verbose) {
      cout<<"card GO "<<n0<<"\t p "<<p<<"\t 1-p "<<un_p<<endl;
    }
    for(j=0;j<nb_comp_var;j++)
    {
      inter=0;
      proba=0;
      ///////WARNING if inter is not an integer... what happens
      for(i=OptimalGroup;i<nb_row;i++)  {
        inter+=supplementary_values[Contrib_index[i]][j];
      }
      
      occ=0;
      for(k=0;k<nb_row;k++)
      occ+=supplementary_values[k][j];
      
      
      if(occ*p*un_p<=10. &&  occ-inter<50)
      {
        for(k=inter+1;k<=occ;k++)
        {
          //long double combi=Fact_div((int)occ,k)*1/Fact((int)occ-k);
          long double combi=Cnp((int)occ,k);
          proba+=combi*pow(p,k)*pow(un_p,occ-k);
        }
      }
      else
      {
        double ecart_type=sqrt(occ*p*un_p);
        double moy=occ*p;
        proba=1.-Normal((inter-moy)/ecart_type);
      }
      string v=supp_var[j];
      cout<<"The variable "<<v;
      if(Typi)
      cout<<" is typical to this class with a risk of "<<proba<<endl;
      else
      cout<<" contributes to this class with a risk of "<<proba<<endl;
      if(verbose)
      {
        cout<<"intersection with the optimal group ";
        cout<<inter<<endl;
      }
      if(min>proba)
      {
        index=j;
        min=proba;
      }
      //return;
    }
    string v=supp_var[index];
    if(Typi)
    cout<<endl<<"The most typical variable is "<<v;
    else
    cout<<endl<<"The most contributive variable is "<<v;
    
    cout<<" with a risk of "<<min<<endl<<endl;
    
    
    
  
  }
  
}



// [[Rcpp::export]]
List hierarchy(NumericMatrix cohesion_matrix, NumericVector list_occurrences_variables, 
NumericMatrix supplementary_variables, NumericMatrix matrix_values, 
LogicalVector contribution_supp, LogicalVector typicality_supp, LogicalVector Verbose) {
  
  
  
  bool verbose=Verbose[0];
  bool contrib_supp=contribution_supp[0];
  bool typi_supp=typicality_supp[0];
  
  if(verbose) cout<<verbose<<endl;
  
  int nb_col=cohesion_matrix.ncol();
  int nb_row=cohesion_matrix.nrow();
  if(verbose) cout<<nb_col<<endl;
  
  List list_names=cohesion_matrix.attr("dimnames");
  List list_names2=matrix_values.attr("dimnames");
  List variables=list_names[0];
  
  //cout<<" list "<<list_names2.size()<<endl;
  List individuals=list_names2[0];
  //cout<<" list2 "<<individuals.size()<<endl;
  
  /*for(int i=0;i<variables.size();i++) {
    string v=variables[i];
    cout<<v<<endl;
  }
  
  for(int i=0;i<individuals.size();i++) {
    string v=individuals[i];
    cout<<v<<endl;
  }*/
  
  
  
  if(verbose)
  Rprintf("Nb col %d\n",nb_col);
  
  
  
  
  
  
  int i,j,k,u,v;
  int x,y;
  double max=-1;
  bool max_found;
  
  vector<int> level(nb_col);
  vector<double> Occurrences_variables(list_occurrences_variables.size());
  for(int i=0;i<list_occurrences_variables.size();i++)
  Occurrences_variables[i]=list_occurrences_variables[i];
  
  vector<int> size_class(nb_col);
  vector<int> variable_left(nb_col);
  vector<int> tabb(nb_col);
  vector<int> variable_right(nb_col);
  vector<int> size_classe(nb_col);
  
  
  //WARNING nb_col+1 needed in case the hierarchy is complete
  TwoDint classes_associated_with(nb_col+1,vector<int> (nb_col+1));
  
  for(i=0;i<nb_col+1;i++)
  for(j=0;j<nb_col+1;j++) classes_associated_with[i][j]=0;
  
  
  
  vector<string> cc(nb_col);
  vector<string> cl(nb_col);
  //char **cc=new char*[nb_col];
  //char **cl=new char*[nb_col];
  //char **string_level=new char*[nb_col];
  vector<string> string_level(nb_col);
  vector<double> cohesion_level(nb_col);
  
  
  vector<int> significant_nodes(nb_col);
  for(i=0;i<nb_col;i++) 
  {
    //cc[i]="";
    //cl[i]=0;
    //significant_nodes[i]=0;
    //string_level[i]=0;
  }
  
  for (i=0;i<nb_col;i++)
  {
    classes_associated_with[i][1]=i;
    size_class[i]=1;
    int length=strlen(variables[i]);
    //cc[i]=new char[10];  //it is only to have the number of the variable
    //cl[i]=new char[length+1];
    //sprintf(cc[i],"%i",i+1);
    std::ostringstream ss;
    long num = i+1;
    ss << num;
    cc[i]= ss.str();//std::to_string(i+1);
    string v=variables[i];
    //sprintf(cl[i],"%s", (char*)v.c_str());
    cl[i]=v.c_str();
  }
  
  int r=nb_col;
  int f=0;
  
  
  
  vector<int> AlreadyInClasse(nb_col);
  for(i=0;i<nb_col;i++) AlreadyInClasse[i]=-999999;
  
  vector<int> GenPairX(nb_col);
  vector<int> GenPairY(nb_col);
  vector<int> Terminal(nb_col);
  for(i=0;i<nb_col;i++) Terminal[i]=1;
  
  
  vector<int> LevelX(r);
  for(i=0;i<r;i++) LevelX[i]=-1;
  
  vector<int> LevelY(r);
  for(i=0;i<r;i++) LevelY[i]=-1;
  
  
  
  
  TwoDdouble Index_cohesion(nb_col,vector<double> (nb_col));
  
  for(int i=0;i<nb_col;i++)
  for(int j=0;j<nb_col;j++) {
    Index_cohesion[i][j]=cohesion_matrix(i,j);
  }
  
  TwoDdouble mat_values(matrix_values.nrow(),vector<double> (matrix_values.ncol()));
  
  for(int i=0;i<matrix_values.nrow();i++)
  for(int j=0;j<matrix_values.ncol();j++) {
    mat_values[i][j]=matrix_values(i,j);
  }
  
  
  
  
  TwoDdouble CurIndex(nb_col,vector<double> (nb_col));
  
  
  while(r>1 && max!=0)
  {
    for (u=0;u<nb_col;u++)
    for (v=0;v<nb_col;v++)
    {
      
      if ( size_class[u]==1 && size_class[v]==1)	CurIndex[u][v]=Index_cohesion[u][v];
      else if (size_class[u]==0 || size_class[v]==0 || u==v) CurIndex[u][v]=0;
      else
      CurIndex[u][v]=Produce(u,v,size_class[u],size_class[v], Index_cohesion, classes_associated_with);
      
      
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
        double c1=Cohesion_classX(x,size_class[x],Index_cohesion,classes_associated_with);
        double c2=Cohesion_classX(u,size_class[u],Index_cohesion,classes_associated_with);
        double impl1=ClassImpli(x,y,u,v,c1,c2,Index_cohesion,classes_associated_with); //impl de la class max
        double impl2=ClassImpli(u,v,x,y,c2,c1,Index_cohesion,classes_associated_with);
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
    
    j=size_class[x];
    
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
      GenericPair(x,y,size_class[x],size_class[y],GenPairX[f],GenPairY[f],Index_cohesion,classes_associated_with);
      size_class[x]=size_class[x]+size_class[u];
      //char * new_s = new char[strlen(cc[x])+2+strlen(cc[u])];
      //strcpy(new_s,cc[x]);
      //strcat(new_s," ");    //espace entre les 2 chaines
      //strcat(new_s,cc[u]);
      ostringstream stringStream;
      stringStream << cc[x]<<" "<<cc[u];
      cc[x] = stringStream.str();
      //delete []cc[u];
      //cc[u]=NULL;
      cc[u]="";
      
      //delete []cc[x];
      //cc[x]=new_s;
      //char * new_s2 = new char[strlen(cl[x])+2+strlen(cl[u])];
      //strcpy(new_s2,cl[x]);
      //strcat(new_s2," ");    //espace entre les 2 chaines
      //strcat(new_s2,cl[u]);
      //delete []cl[u];
      //cl[u]=NULL;
      //delete []cl[x];
      //cl[x]=new_s2;
      ostringstream stringStream2;
      stringStream2 << cl[x]<<" "<<cl[u];
      cl[x] = stringStream2.str();
      cl[u]="";
      
      r=r-1;
      for (k=j+1;k<=j+size_class[u];k++)
      classes_associated_with[x][k]=classes_associated_with[u][k-j];
      j=j+size_class[u];
      size_class[u]=0;
    }
    }
    if (max!=0)
    {
      variable_left[f]=classes_associated_with[x][1];
      variable_right[f]=classes_associated_with[x][size_class[x]];
      size_classe[f]=size_class[x];
      //char *new_s=new char[strlen(cc[x])+3];  //3 a cause des ()
      //char *new_s2=new char[strlen(cl[x])+3];
      //wsprintf(new_s,"(%s)",cc[x]);         //on met la classe formée entre ()
      //strcpy(new_s,"(");
      //strcat(new_s,cc[x]);    //espace entre les 2 chaines
      //strcat(new_s,")");
      //delete []cc[x];
      //cc[x]=new_s;
      ostringstream stringStream;
      stringStream << "(" << cc[x] << ")";
      cc[x] = stringStream.str();
      
      
      //strcpy(new_s2,"(");
      //strcat(new_s2,cl[x]);    //espace entre les 2 chaines
      //strcat(new_s2,")");
      //delete []cl[x];
      //cl[x]=new_s2;
      ostringstream stringStream2;
      stringStream2 << "(" << cl[x] << ")";
      cl[x] = stringStream2.str();
      
      double a=Cohesion_classX(x,size_class[x],Index_cohesion,classes_associated_with);
      //Rprintf("Classification %d : %s  Cohesion %f\n",(f+1),cl[x],a);
      if(verbose) cout<<"Classification "<<f+1<<" : "<<cl[x]<<" Cohesion "<<a<<endl;
      //string_level[f]=new char[strlen(cl[x])+3];
      //strcpy(string_level[f],cl[x]);
      string_level[f]=cl[x];
      cohesion_level[f]=a;
      //tab_cohe[f]=a;
      if (max) f++;
      
    }
    
    
    
    
    
  }
  
  
  j=0;
  k=0;
//  for(i=0;i<nb_col;i++)
//  {
//    if(cc[i]) j+=strlen(cc[i])+1;
//  }
//  for(i=0;i<nb_col;i++)
//  {
//    if(cl[i]) k+=strlen(cl[i])+1;
//  }
  
  ostringstream stringStream;
  ostringstream stringStream2;
  
 
  
//  char *chc=new char[j+1];
//  chc[0]='\0';
//  char *chl=new char[k+1];
//  chl[0]='\0';
  for(i=0;i<nb_col;i++)
  {
    if(size_class[i])
    {
//      strcat(chc,cc[i]);
//      strcat(chc," ");
      stringStream << cc[i]<<" ";
//      strcat(chl,cl[i]);
//      strcat(chl," ");
      stringStream2 << cl[i]<<" ";
    }
  }
  
  string chc = stringStream.str(); 
  string chl = stringStream2.str(); 
  
  
  
  SignificantLevel(Index_cohesion, nb_col, Occurrences_variables,f,variable_left,variable_right,size_classe,classes_associated_with,significant_nodes,verbose);
  
  
    
  if(supplementary_variables.ncol()>0) {
    if(contrib_supp) {
      contributiveCategories(supplementary_variables,Index_cohesion,f,GenPairX,GenPairY,LevelX,LevelY,
      mat_values,0, matrix_values.ncol(), matrix_values.nrow(), individuals,string_level,true,verbose);   //false means hierarchy     0 means contrib
    }
    if(typi_supp) {
      contributiveCategories(supplementary_variables,Index_cohesion,f,GenPairX,GenPairY,LevelX,LevelY,
      mat_values,1, matrix_values.ncol(), matrix_values.nrow(), individuals,string_level,true,verbose);   //false means hierarchy     1 means typicality
    } 
  }
  
  
  List results(8);
  List listClasses(2);
  listClasses[0]=string(chc);
  listClasses[1]=string(chl);
  results[0]=listClasses;
  
  IntegerVector Rvariable_left(f);
  for(i=0;i<f;i++)
  Rvariable_left[i]=variable_left[i]+1;  //+1 because in R indexes start at 1
  results[1]=Rvariable_left;
  
  IntegerVector Rvariable_right(f);
  for(i=0;i<f;i++)
  Rvariable_right[i]=variable_right[i]+1;  //idem
  results[2]=Rvariable_right;
  
  IntegerVector RnbLevel(1);
  RnbLevel[0] = f;
  results[3]=RnbLevel;
  
  
  IntegerVector Rsignificant_nodes(nb_col);
  for(i=0;i<nb_col;i++)
  Rsignificant_nodes[i]=significant_nodes[i];
  results[4]=Rsignificant_nodes;
  
  
  IntegerVector Rfinal_nodes(nb_col);
  for(i=0;i<nb_col;i++)
  Rfinal_nodes[i]=size_class[i];
  results[5]=Rfinal_nodes;
  
  
  NumericVector Rvariable_cohesion_level(f);
  for(i=0;i<f;i++) {
    Rvariable_cohesion_level[i]=cohesion_level[i];  
  }
  results[6]=Rvariable_cohesion_level; //cohesion indices list
  
  results[7]=string_level; //list of classes  
  
  
  
//  for(i=0;i<nb_col;i++)
//  if(cl[i]) delete []cl[i];
//  delete []cl;
//  for(i=0;i<nb_col;i++)
//  if(cc[i]) delete []cc[i];
//  delete []cc;
//  for(i=0;i<nb_col;i++)
//  if(string_level[i]) delete []string_level[i];
  
  return results;
  
  
}










// [[Rcpp::export]]
List similarity(NumericMatrix  similarity_matrix, NumericVector list_occurrences_variables, 
NumericMatrix supplementary_variables, NumericMatrix matrix_values, 
LogicalVector contribution_supp, LogicalVector typicality_supp, LogicalVector Verbose) {
  
  
  bool verbose=Verbose[0];
  bool contrib_supp=contribution_supp[0];
  bool typi_supp=typicality_supp[0];
  
  
  //cout<<verbose<<endl;
  
  int nb_col=similarity_matrix.ncol();
  int nb_row=similarity_matrix.nrow();
  //cout<<nb_col<<endl;
  
  List list_names=similarity_matrix.attr("dimnames");
  List list_names2=matrix_values.attr("dimnames");
  List variables=list_names[0];
  
  //cout<<" list "<<list_names2.size()<<endl;
  List individuals=list_names2[0];
  //cout<<" list2 "<<individuals.size()<<endl;
  
  for(int i=0;i<variables.size();i++) {
    string v=variables[i];
    if(verbose) cout<<v<<endl;
  }
  
  for(int i=0;i<individuals.size();i++) {
    string v=individuals[i];
    if(verbose) cout<<v<<endl;
  }
  
  
  
  int i,j,k,u,v;
  int x,y;
  double max;
  bool max_found;
  
  vector<int> level(nb_col);
  
  vector<double> Occurrences_variables(list_occurrences_variables.size());
  for(int i=0;i<list_occurrences_variables.size();i++)
  Occurrences_variables[i]=list_occurrences_variables[i];
  
  
  vector<int> size_class(nb_col);
  vector<int> variable_left(nb_col);
  vector<int> tabb(nb_col);
  vector<int> variable_right(nb_col);
  vector<int> size_classe(nb_col);
  
  
  
  //WARNING classes_associated_with should be of size nb_col+1 because the similarity can be total
  TwoDint classes_associated_with(nb_col+1,vector<int> (nb_col+1));
  
  for(i=0;i<nb_col+1;i++) {
    for(j=0;j<nb_col+1;j++) {
      classes_associated_with[i][j]=0;
    }
  }
  
  
  
  
  //return(R_NilValue);
  vector<string> cc(nb_col);
  vector<string> cl(nb_col);
  //char **cc=new char*[nb_col];
  //char **cl=new char*[nb_col];
  vector<int> significant_nodes(nb_col);
  vector<double> similarity_level(nb_col);
  vector<string> string_level(nb_col);
  
  //char **string_level = new char*[nb_col];
  for(i=0;i<nb_col;i++) 
  {
//    cc[i]=0;
//    cl[i]=0;
//    significant_nodes[i]=0;
    //string_level[i]='\0;
  }
  
  
  for (i=0;i<nb_col;i++)
  {
    classes_associated_with[i][1]=i;
    size_class[i]=1;
    //int length=strlen(variables[i]);
    //cc[i]=new char[10];  //it is only to have the number of the variable
    //cl[i]=new char[length+1];
    //sprintf(cc[i],"%i",i+1);
    std::ostringstream ss;
    long num = i+1;
    ss << num;
    
    cc[i]=ss.str();//std::to_string(i+1);
    string v=variables[i];
    //sprintf(cl[i],"%s", (char*)v.c_str());
    cl[i]=v.c_str();
  }
  
  int r=nb_col;
  
  int f=0;
  
  
  
  vector<int> AlreadyInClasse(nb_col);
  for(i=0;i<nb_col;i++) AlreadyInClasse[i]=-999999;
  
  vector<int> GenPairX(nb_col);
  vector<int> GenPairY(nb_col);
  
  vector<int>	Terminal(nb_col);
  for(i=0;i<nb_col;i++) Terminal[i]=1;
  
  
  vector<int> LevelX(r);
  for(i=0;i<r;i++) LevelX[i]=-1;
  
  vector<int> LevelY(r);
  for(i=0;i<r;i++) LevelY[i]=-1;
  
  
  
  
  TwoDdouble Index_simi(nb_col,vector<double> (nb_col));
  
  for(int i=0;i<nb_col;i++)
  for(int j=0;j<nb_col;j++) {
    Index_simi[i][j]=similarity_matrix(i,j);
  }
  
  TwoDdouble mat_values(matrix_values.nrow(),vector<double> (matrix_values.ncol()));
  
  for(int i=0;i<matrix_values.nrow();i++)
  for(int j=0;j<matrix_values.ncol();j++) {
    mat_values[i][j]=matrix_values(i,j);
  }
  
  TwoDdouble CurIndex(nb_col,vector<double> (nb_col));
  
  
  
  
  do
  {
    for (u=0;u<nb_col-1;u++)
    for (v=u+1;v<nb_col;v++) {
      if ( size_class[u]==1 && size_class[v]==1)	CurIndex[u][v]=Index_simi[u][v];
      else if (size_class[u]==0 || size_class[v]==0) CurIndex[u][v]=0;
      else {
        x=classes_associated_with[u][1];
        y=classes_associated_with[v][1];
        max=Index_simi[x][y];
        for (j=1;j<=size_class[u];j++)
        for (k=1;k<=size_class[v];k++)
        {
          double t=Index_simi[classes_associated_with[u][j]][classes_associated_with[v][k]];  
          if (max<t) max=t;
          t=pow(max,size_class[u]);
          CurIndex[u][v]=pow(t,size_class[v]);
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
      
      if (size_class[u]==1 && size_class[v]==1) CurIndex[u][v]=Index_simi[u][v];
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
    if (size_class[u]!=0 && size_class[v]!=0)
    if (CurIndex[u][v]==max)
    {
      tabb[u]=u;
      tabb[v]=v;
    }
    
    
    j=size_class[x];
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
          GenericPair(x,y,size_class[x],size_class[y],GenPairX[f],GenPairY[f], Index_simi, classes_associated_with);
          
          
          level[u]=f;
          size_class[x]=size_class[x]+size_class[u];
//          char * new_s = new char[strlen(cc[x])+2+strlen(cc[u])];
//          strcpy(new_s,cc[x]);
//          strcat(new_s," ");    //espace entre les 2 chaines
//          strcat(new_s,cc[u]);
//          delete []cc[u];
//          cc[u]=0;
//          delete []cc[x];
//          cc[x]=new_s;
//          char * new_s2 = new char[strlen(cl[x])+2+strlen(cl[u])];
//          strcpy(new_s2,cl[x]);
//          strcat(new_s2," ");    //espace entre les 2 chaines
//          strcat(new_s2,cl[u]);
//          delete []cl[u];
//          cl[u]=NULL;
//          delete []cl[x];
//          cl[x]=new_s2;
          ostringstream stringStream;
          stringStream << cc[x]<<" "<<cc[u];
          cc[x] = stringStream.str();
          cc[u]="";
          ostringstream stringStream2;
          stringStream2 << cl[x]<<" "<<cl[u];
          cl[x] = stringStream2.str();
          cl[u]="";
          r--;
          for (k=j+1;k<=j+size_class[u] ;k++) {
            classes_associated_with[x][k]=classes_associated_with[u][k-j];
          }
          j=j+size_class[u];
          size_class[u]=0;
        }
      }
      variable_left[f]=classes_associated_with[x][1];
      variable_right[f]=classes_associated_with[x][size_class[x]];
      if(verbose)
        Rprintf("Level %d variable_left %d variable_right %d\n",f,variable_left[f],variable_right[f]);
      size_classe[f]=size_class[x];			
      
//      char *new_s=new char[strlen(cc[x])+3];  //3 a cause des ()
//      char *new_s2=new char[strlen(cl[x])+3];
      
//      sprintf(new_s,"(%s)",cc[x]);         //on met la classe formée entre ()
//      //attention wsprintf est limite a 1024 caractere
//      strcpy(new_s,"(");
//      strcat(new_s,cc[x]);    //espace entre les 2 chaines
//      strcat(new_s,")");
//      delete []cc[x];
//      cc[x]=new_s;
//      strcpy(new_s2,"(");
//      strcat(new_s2,cl[x]);    //espace entre les 2 chaines
//      strcat(new_s2,")");
//      delete []cl[x];
//      cl[x]=new_s2;
      ostringstream stringStream;
      stringStream << "(" << cc[x] << ")";
      cc[x] = stringStream.str();
      ostringstream stringStream2;
      stringStream2 << "(" << cl[x] << ")";
      cl[x] = stringStream2.str();

      //Rprintf("Classification %d : %s similarity %f\n",f+1,cl[x],max);
      if(verbose) cout<<"Classification "<<f+1<<" : "<<cl[x]<<" Similarity "<<max<<endl;
      //string_level[f]=new char[strlen(cl[x])+3];
      //strcpy(string_level[f],cl[x]);
      string_level[f]=cl[x];
      similarity_level[f]=max;
      f++;
      
      
    }
    
    
  }while(r>1 && max>1e-12);    
  
  
  
  
  j=0;
  k=0;
//  for(i=0;i<nb_col;i++)
//  {
//    if(cc[i]) j+=strlen(cc[i])+1;
//  }
//  for(i=0;i<nb_col;i++)
//  {
//    if(cc[i]) k+=strlen(cl[i])+1;
//  }
  
//  char *chc=new char[j+1];
//  chc[0]='\0';
//  char *chl=new char[k+1];
//  chl[0]='\0';

  ostringstream stringStream;
  ostringstream stringStream2;
  for(i=0;i<nb_col;i++)
  {
    //classes not selected are suppressed
    if(size_class[i] && AlreadyInClasse[i]>=0)
    {
//      strcat(chc,cc[i]);
//      strcat(chc," ");
//      strcat(chl,cl[i]);
//      strcat(chl," ");
        stringStream << cc[i]<<" ";
        stringStream2 << cl[i]<<" ";
    }
  }
  string chc = stringStream.str(); 
  string chl = stringStream2.str(); 
  
  
  SignificantLevel(Index_simi, nb_col, Occurrences_variables,f,variable_left,variable_right,size_classe,classes_associated_with,significant_nodes,verbose);
  
  if(supplementary_variables.ncol()>0) {
    if(contrib_supp) {
      contributiveCategories(supplementary_variables,Index_simi,f,GenPairX,GenPairY,LevelX,LevelY,
      mat_values,0, matrix_values.ncol(), matrix_values.nrow(), individuals,string_level,false,verbose);   //false means similarity     0 means contrib
    }
    if(typi_supp) {
      contributiveCategories(supplementary_variables,Index_simi,f,GenPairX,GenPairY,LevelX,LevelY,
      mat_values,1, matrix_values.ncol(), matrix_values.nrow(), individuals,string_level,false,verbose);   //false means similarity     1 means typicality
    }
  }
  
  
  
  List results(7);
  List listClasses(2);
  listClasses[0]=string(chc);
  listClasses[1]=string(chl);
  results[0]=listClasses;
  
  IntegerVector Rvariable_left(f);
  for(i=0;i<f;i++)
  Rvariable_left[i]=variable_left[i]+1;  //+1 because in R indexes start at 1
  results[1]=Rvariable_left;
  
  IntegerVector Rvariable_right(f);
  for(i=0;i<f;i++)
  Rvariable_right[i]=variable_right[i]+1;  //idem
  results[2]=Rvariable_right;
  
  IntegerVector RnbLevel(1);
  RnbLevel[0] = f;
  results[3]=RnbLevel;
  
  
  IntegerVector Rsignificant_nodes(nb_col);
  for(i=0;i<nb_col;i++)
  Rsignificant_nodes[i]=significant_nodes[i];
  results[4]=Rsignificant_nodes;
  
  NumericVector Rvariable_similarity_level(f);
  for(i=0;i<f;i++) {
    Rvariable_similarity_level[i]=similarity_level[i];  
  }
  results[5]=Rvariable_similarity_level; //similarity indices list
  
  results[6]=string_level; //list of classes
  
  
  
//  for(i=0;i<nb_col;i++)
//  if(cl[i]) delete []cl[i];
//  delete []cl;
//  for(i=0;i<nb_col;i++)
//  if(cc[i]) delete []cc[i];
//  for(i=0;i<nb_col;i++) {
//    if(string_level[i]) 
//    delete []string_level[i];
//  }
//  delete []string_level;
//  
//  
//  
//  
//  delete []cc;
  
  
  return results;
  
  
}
