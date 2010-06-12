#include "ast.h"
#include <iostream>

using namespace std;

namespace SPL { namespace AST {

void File::LambdaLiftFuncs() {
  vector<Func*> newFuncs;

  for (vector<Func*>::const_iterator it=Funcs.begin(); it!=Funcs.end(); it++)
    (*it)->LambdaLift(newFuncs);

  string num;
  num += newFuncs.size();
  DEBUG(dbgs() << "Lifted " << num << " functions.\n");
  Funcs.insert(Funcs.end(), newFuncs.begin(), newFuncs.end());
}

Expr* Expr::LambdaLift(vector<Func*> &newFuncs) {
  return this;
}

Expr* UnaryOp::LambdaLift(vector<Func*> &newFuncs) {
  SubExpr = SubExpr->LambdaLift(newFuncs);
  return this;
}

Expr* BinaryOp::LambdaLift(vector<Func*> &newFuncs) {
  LHS = LHS->LambdaLift(newFuncs);
  RHS = RHS->LambdaLift(newFuncs);
  return this;
}

Expr* Binding::LambdaLift(vector<Func*> &newFuncs) {
  Body = Body->LambdaLift(newFuncs);
  return this;
}

Expr* If::LambdaLift(vector<Func*> &newFuncs) {
  Cond = Cond->LambdaLift(newFuncs);
  Then = Then->LambdaLift(newFuncs);
  Else = Else->LambdaLift(newFuncs);
  return this;
}

Expr* Call::LambdaLift(vector<Func*> &newFuncs) {
  for (int i=0; i < Args.size(); i++) {
    Args[i] = Args[i]->LambdaLift(newFuncs);
  }
  return this;
}

Expr* Func::LambdaLift(vector<Func*> &newFuncs) {
  Body = Body->LambdaLift(newFuncs);

  if (Context == NULL) {
    // Top-level function.
    return this;
  } else {
    // Inner function definition. Lift and replace with closure.
    set<string> newB(Args.begin(), Args.end());
    set<string> *freeVars = FindFreeVars(&newB);
    vector<string> newArgs(freeVars->begin(), freeVars->end());
    newArgs.insert(newArgs.end(), Args.begin(), Args.end());
    string newName("$FromInner$" + Name);

    Body->LambdaLift(newFuncs);
    Body->RewriteBinding(Name, newName);
    Context->RewriteBinding(Name, newName);
    Context->LambdaLift(newFuncs);
    Func *newFunc = new Func(
      newName, newArgs, ArgSTypeNames, RetSTypeName, *Body, NULL, Pureness);

    vector<string> activationRecord;
    set<string>::const_iterator it;
    for (it=freeVars->begin(); it!=freeVars->end(); it++)
      activationRecord.push_back(*it);
    Closure *closure = new Closure(newName, activationRecord, newFunc);
    newFuncs.push_back(newFunc);

    Binding *b = new Binding(Name, *closure, false);
    b->setBody(*Context);
    return b;
  }
}

/////////////////////////////////////////////////////////////////////

void Expr::RewriteBinding(string &OldName, string &NewName) {
}

void Variable::RewriteBinding(string &OldName, string &NewName) {
  if (Name == OldName)
    Name = NewName;
}

void UnaryOp::RewriteBinding(string &OldName, string &NewName) {
  SubExpr->RewriteBinding(OldName, NewName);
}

void BinaryOp::RewriteBinding(string &OldName, string &NewName) {
  LHS->RewriteBinding(OldName, NewName);
  RHS->RewriteBinding(OldName, NewName);
}

void Binding::RewriteBinding(string &OldName, string &NewName) {
  Init->RewriteBinding(OldName, NewName);
  if (OldName != Name) {
    // Only rewrite if this Bind is not shadowing the definition.
    Body->RewriteBinding(OldName, NewName);
  }
}

void If::RewriteBinding(string &OldName, string &NewName) {
  Cond->RewriteBinding(OldName, NewName);
  Then->RewriteBinding(OldName, NewName);
  Else->RewriteBinding(OldName, NewName);
}

void Call::RewriteBinding(string &OldName, string &NewName) {
  if (CalleeName == OldName)
    CalleeName = NewName;
  for (vector<Expr*>::const_iterator it=Args.begin(); it!=Args.end(); it++) {
    (*it)->RewriteBinding(OldName, NewName);
  }
}

void Func::RewriteBinding(string &OldName, string &NewName) {
  bool bindsOldName = Name == OldName;
  for (vector<string>::const_iterator it=Args.begin(); it!=Args.end(); it++) {
    if (*it == OldName)
      bindsOldName = true;
  }
  if (!bindsOldName) {
    Body->RewriteBinding(OldName, NewName);
    Context->RewriteBinding(OldName, NewName);
  }
}

void Closure::RewriteBinding(string &OldName, string &NewName) {
  if (FuncName == OldName)
    FuncName = NewName;
  // TODO: go through ActivationRecord, replace any matching bindings.
}


/////////////////////////////////////////////////////////////////////

set<string> *Expr::FindFreeVars(set<string> *bindings) {
  return new set<string>();
}

set<string> *Variable::FindFreeVars(set<string> *bindings) {
  set<string> *ret = new set<string>();
  if (bindings->find(Name) == bindings->end())
    ret->insert(Name);
  return ret;
}

set<string> *UnaryOp::FindFreeVars(set<string> *bindings) {
  return SubExpr->FindFreeVars(bindings);
}

set<string> *BinaryOp::FindFreeVars(set<string> *bindings) {
  set<string> *v1 = LHS->FindFreeVars(bindings);
  set<string> *v2 = RHS->FindFreeVars(bindings);
  v1->insert(v2->begin(), v2->end());
  return v1;
}

set<string> *Binding::FindFreeVars(set<string> *bindings) {
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
