#include "ast.h"
#include <list>
#include <iostream>

namespace SPL { namespace AST {

extern map<string,SType*> NamedTypes; // TODO: hackish nonsense

void Number::TypeInfer(multimap<Expr*,Expr*> &eqns, map<Expr*,SType*> &tys) {
  tys[this] = new Int32();
}
void Variable::TypeInfer(multimap<Expr*,Expr*> &eqns, map<Expr*,SType*> &tys) {
  eqns.insert(pair<Expr*,Expr*>(this, Binding));
}
void UnaryOp::TypeInfer(multimap<Expr*,Expr*> &eqns, map<Expr*,SType*> &tys) {
  eqns.insert(pair<Expr*,Expr*>(this, SubExpr));
  SubExpr->TypeInfer(eqns, tys);
}
void BinaryOp::TypeInfer(multimap<Expr*,Expr*> &eqns, map<Expr*,SType*> &tys) {
  eqns.insert(pair<Expr*,Expr*>(this, LHS));
  eqns.insert(pair<Expr*,Expr*>(this, RHS));
  LHS->TypeInfer(eqns, tys);
  RHS->TypeInfer(eqns, tys);
}
void Eq::TypeInfer(multimap<Expr*,Expr*> &eqns, map<Expr*,SType*> &tys) {
  tys[this] = new SBool();
  LHS->TypeInfer(eqns, tys);
  RHS->TypeInfer(eqns, tys);
}
void Seq::TypeInfer(multimap<Expr*,Expr*> &eqns, map<Expr*,SType*> &tys) {
  eqns.insert(pair<Expr*,Expr*>(this,RHS));
  LHS->TypeInfer(eqns, tys);
  RHS->TypeInfer(eqns, tys);
}
void Member::TypeInfer(multimap<Expr*,Expr*> &eqns, map<Expr*,SType*> &tys) {
  // TODO
}
void Binding::TypeInfer(multimap<Expr*,Expr*> &eqns, map<Expr*,SType*> &tys) {
  Init->TypeInfer(eqns, tys);
  InitReg->TypeInfer(eqns, tys);
  Body->TypeInfer(eqns, tys);
  eqns.insert(pair<Expr*,Expr*>(this,Body));
}
void If::TypeInfer(multimap<Expr*,Expr*> &eqns, map<Expr*,SType*> &tys) {
  Cond->TypeInfer(eqns, tys);
  Then->TypeInfer(eqns, tys);
  Else->TypeInfer(eqns, tys);
  tys[Cond] = new SBool();
  eqns.insert(pair<Expr*,Expr*>(this,Then));
  eqns.insert(pair<Expr*,Expr*>(this,Else));
}
void Call::TypeInfer(multimap<Expr*,Expr*> &eqns, map<Expr*,SType*> &tys) {
  // Match each Arg to the associate Func register.
  vector<RegisterFunArg*> argRegs;
  if (Closure *cl = dynamic_cast<Closure*>(Callee)) {
    cl->getArgRegs(argRegs);
    tys[this] = cl->getFunction()->getFunctionSType()->getReturnType();
  } else if (Func *fn = dynamic_cast<Func*>(Callee)) {
    fn->getArgRegs(argRegs);
    tys[this] = fn->getFunctionSType()->getReturnType();
  } else {
    std::cerr << "Call to " << CalleeName << " is not function nor closure."
      << std::endl;
    exit(1);
  }

  assert(argRegs.size() == Args.size());
  for (unsigned i=0, e=Args.size(); i != e; ++i) {
    eqns.insert(pair<Expr*,Expr*>(Args[i], argRegs[i]));
    Args[i]->TypeInfer(eqns, tys);
  }
}
void Register::TypeInfer(multimap<Expr*,Expr*> &eqns, map<Expr*,SType*> &tys) {
  eqns.insert(pair<Expr*,Expr*>(this,Source));
}
void RegisterFunArg::TypeInfer(multimap<Expr*,Expr*>&, map<Expr*,SType*> &tys){
  tys[this] = ThisType; // Set in the Bind phase by func type signature.
}
void Func::TypeInfer(multimap<Expr*,Expr*> &eqns, map<Expr*,SType*> &tys) {
  tys[Body] = RetSType;
  Body->TypeInfer(eqns, tys);
}
void Closure::TypeInfer(multimap<Expr*,Expr*> &eqns, map<Expr*,SType*> &tys) {
  SFunctionType *ft = FuncRef->getFunctionSType();
  vector <SType*> funArgs = ft->getArgs();
  vector <SType*> closureArgs;

  for (unsigned i=ActivationRecord.size(), e=funArgs.size(); i != e; ++i)
    closureArgs.push_back(funArgs[i]);

  string Name("$SomeClosure$");
  tys[this] = new SFunctionType(Name, closureArgs, ft->getReturnType());

  vector<RegisterFunArg*> argRegs;
  FuncRef->getArgRegs(argRegs);
  unsigned idx = 0;
  map<string,Expr*>::const_iterator it;
  for (it=ActivationRecord.begin(); it!=ActivationRecord.end(); ++it, ++idx)
    eqns.insert(pair<Expr*,Expr*>((*it).second, argRegs[idx]));
}
void Constructor::TypeInfer(multimap<Expr*,Expr*>&eqns, map<Expr*,SType*> &tys){
  SType *ty = NamedTypes[STypeName];
  if (ty == NULL) {
    std::cerr << "Unknown type: " << STypeName << std::endl;
    exit(1);
  }
  SStructType *sty = dynamic_cast<SStructType*>(ty);
  if (sty == NULL) {
    std::cerr << "Constructor " << STypeName << " bound to wrong kind: ";
    ty->dump();
    std::cerr << std::endl;
    exit(1);
  }
  tys[this] = sty;

  vector<SType*> argTys;
  sty->getElementSTypes(argTys);
  for (unsigned i=0, e=Args.size(); i != e; ++i) {
    Args[i]->TypeInfer(eqns, tys);
    tys[Args[i]] = argTys[i]; // TODO: check for conflict in tys.
  }
}


// TODO: put these in a different namespace, maybe SPL::TypeInference

void TypeUnification(multimap<Expr*,Expr*> &eqnsMap, map<Expr*,SType*> &tys) {
  std::list<pair<Expr*,Expr*> > eqns;
  {
    multimap<Expr*,Expr*>::const_iterator i;
    for (i=eqnsMap.begin(); i != eqnsMap.end(); i++)
      eqns.push_back(*i);
  }

  unsigned noMatchesIn = 0;
  while (eqns.size() > 0 && noMatchesIn < eqns.size() * 2) {
    pair<Expr*,Expr*> eqn = eqns.front();
    eqns.pop_front();

    if (tys.count(eqn.first) > 0) {
      tys[eqn.second] = tys[eqn.first];
      noMatchesIn = 0;
    } else if (tys.count(eqn.second) > 0) {
      tys[eqn.first] = tys[eqn.second];
      noMatchesIn = 0;
    } else {
      eqns.push_back(eqn);
      noMatchesIn++;
    }
  }

  if (eqns.size() > 0) {
    std::vector<pair<Expr*,Expr*> > eqnsV(eqns.begin(), eqns.end());
    std::cerr << "Unable to resolve all types." << std::endl;
    exit(1);
  }
}

void TypePopulation(map<Expr*,SType*> &tys) {
  for (map<Expr*,SType*>::const_iterator i=tys.begin(); i!=tys.end(); i++)
    i->first->setSType(i->second);
}


}};
