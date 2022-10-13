#include <cstdio>
#include <cstdlib>

class Vector{
private:
  int length;
public:
  double * vec;
  //Constructors
  Vector();
  Vector(int);
  ~Vector();
  //Methods-functions
  int GetLengthVector(void);
  void SetLengthVector(int);
  friend int GetLength(Vector &);
};

//define methods and constructors/destructor outside the class declaration
Vector::Vector(){
  length=4;
  vec = (double*) calloc(length, sizeof(double));
}
Vector::Vector(int a){
  length=a;
  vec = (double*) calloc(length, sizeof(double));
}
Vector::~Vector(){
  if(vec!=NULL){
    free(vec);
  }
}
int Vector::GetLengthVector(void){
  return length;
}
void Vector::SetLengthVector(int l){
  length=l;
  vec = (double*) realloc(vec,length*sizeof(double));
}

//Friend function of class Vector
int GetLength(Vector & Vect){
  return Vect.length;
}

// main function
int main(void){

  //Create an object of Vector with default constructor
  Vector Vec1;
  //Vec1.length=2;  //cant work length is private
  Vec1.SetLengthVector(2);
  printf("Vect1.length = %d\n", Vec1.GetLengthVector());

  //Create an object of Vector with second constructor
  Vector Vec2(8);
  printf("Vect2.length = %d\n", GetLength(Vec2));

  return 0;
}
