#include "ast.h"

using namespace std;

namespace SPL { namespace AST {

set<string> *Expr::FindFreeVars(set<string> *bindings) {
  return new set<string>();
}

set<string> *Variable::FindFreeVars(set<string> *bindings) {
  set<string> *ret = new set<string>();
  if (bindings->find(Name) == bindings->end())
    ret->insert(Name);
  return ret;
}

set<string> *Not::FindFreeVars(set<string> *bindings) {
  return SubExpr->FindFreeVars(bindings);
}

set<string> *BinaryOp::FindFreeVars(set<string> *bindings) {
  set<string> *v1 = LHS->FindFreeVars(bindings);
  set<string> *v2 = RHS->FindFreeVars(bindings);
  v1->insert(v2->begin(), v2->end());
  return v1;
}

set<string> *Bind::FindFreeVars(set<string> *bindings) {
  set<string> *v1 = Init->FindFreeVars(bindings);
  set<string> *newB = new set<string>();
  newB->insert(bindings->begin(), bindings->end());
  newB->insert(Name);
  set<string> *v2 = Body->FindFreeVars(newB);
  v1->insert(v2->begin(), v2->end());
  return v1;
}

set<string> *If::FindFreeVars(set<string> *bindings) {
  set<string> *v1 = Cond->FindFreeVars(bindings);
  set<string> *v2 = Then->FindFreeVars(bindings);
  set<string> *v3 = Else->FindFreeVars(bindings);
  v1->insert(v2->begin(), v2->end());
  v1->insert(v3->begin(), v3->end());
  return v1;
}

set<string> *Call::FindFreeVars(set<string> *bindings) {
  set<string> *ret = new set<string>();
  for (vector<Expr*>::const_iterator it=Args.begin(); it!=Args.end(); it++) {
    set<string> *v1 = (*it)->FindFreeVars(bindings);
    ret->insert(v1->begin(), v1->end());
  }
  return ret;
}

set<string> *Func::FindFreeVars(set<string> *bindings) {
  set<string> *inFuncB = new set<string>();
  inFuncB->insert(bindings->begin(), bindings->end());
  inFuncB->insert(Args.begin(), Args.end());
  set<string> *v1 = Body->FindFreeVars(inFuncB);

  set<string> *inContext = new set<string>();
  inContext->insert(bindings->begin(), bindings->end());
  inContext->insert(Name);
  set<string> *v2 = Context->FindFreeVars(inContext);

  v1->insert(v2->begin(), v2->end());
  return v1;
}

}};
