#include "ast.h"
#include <list>
#include <iostream>

namespace SPL { namespace AST {

extern map<string,SType*> NamedTypes; // TODO: hackish nonsense

void Number::TypeInfer(TypeInferer &inferer) {
  inferer.ty(this, new Int32());
}
void StringLiteral::TypeInfer(TypeInferer &inferer) {
  inferer.ty(this, new SString());
}
void JoinString::TypeInfer(TypeInferer &inferer) {
  inferer.ty(this, new SString());
  LHS->TypeInfer(inferer);
  RHS->TypeInfer(inferer);
}
void Variable::TypeInfer(TypeInferer &inferer) {
  inferer.eqn(this, Binding);
}
void UnaryOp::TypeInfer(TypeInferer &inferer) {
  inferer.eqn(this, SubExpr);
  SubExpr->TypeInfer(inferer);
}
void BinaryOp::TypeInfer(TypeInferer &inferer) {
  inferer.eqn(this, LHS);
  inferer.eqn(this, RHS);
  LHS->TypeInfer(inferer);
  RHS->TypeInfer(inferer);
}
void Eq::TypeInfer(TypeInferer &inferer) {
  inferer.ty(this, new SBool());
  LHS->TypeInfer(inferer);
  RHS->TypeInfer(inferer);
}
void Seq::TypeInfer(TypeInferer &inferer) {
  inferer.eqn(this,RHS);
  LHS->TypeInfer(inferer);
  RHS->TypeInfer(inferer);
}
void Assign::TypeInfer(TypeInferer &inferer) {
  inferer.eqn(this, RHS);
  LHS->TypeInfer(inferer);
  RHS->TypeInfer(inferer);
}
void ArrayAccess::TypeInfer(TypeInferer &inferer) {
  inferer.arrayAccess(this);
  LHS->TypeInfer(inferer);
  RHS->TypeInfer(inferer);
}
void ArrayAccess::TypeInferSecondPass() {
  SArray *sty = getSourceSType();
  setSType(sty->getContained());
}
void Member::TypeInfer(TypeInferer &inferer) {
  inferer.member(this);
  Source->TypeInfer(inferer);
  // TODO even with local type inference, this needs to happen in a
  // pass after initial substition, when the type of the Sourceis known.
}
void Member::TypeInferSecondPass() {
  SStructType *sty = getSourceSType();
  unsigned fieldIndex = sty->getIndex(FieldName);
  if (fieldIndex < 0) {
    std::cerr << "Unknown field `" << FieldName
      << "' on `" << sty->getName() << "'" << std::endl;
    exit(1);
  }
  setSType(sty->getSType(fieldIndex));
}
void Binding::TypeInfer(TypeInferer &inferer) {
  Init->TypeInfer(inferer);
  InitReg->TypeInfer(inferer);
  Body->TypeInfer(inferer);
  inferer.eqn(this,Body);
}
void If::TypeInfer(TypeInferer &inferer) {
  Cond->TypeInfer(inferer);
  Then->TypeInfer(inferer);
  Else->TypeInfer(inferer);
  inferer.ty(Cond, new SBool());
  inferer.eqn(this,Then);
  inferer.eqn(this,Else);
}
void Call::TypeInfer(TypeInferer &inferer) {
  Func *fn;
  if (Closure *cl = dynamic_cast<Closure*>(Callee)) {
    fn = cl->getFunc();
  } else if (Func *fun = dynamic_cast<Func*>(Callee)) {
    fn = fun;
  } else {
    std::cerr << "Call to " << CalleeName << " is not function nor closure."
      << std::endl;
    exit(1);
  }

  for (unsigned i=0, e=Args.size(); i != e; ++i)
    Args[i]->TypeInfer(inferer);

  SFunctionType *funTy = fn->getFunctionSType();
  SType *retTy = funTy->getReturnType();
  if (SGenericType *ret = dynamic_cast<SGenericType*>(retTy)) {
    // Match the return type to one of the input types to the generic.
    vector<SType*> argTys = funTy->getArgs();
    for (unsigned i=0, e=argTys.size(); i != e; ++i) {
      if (argTys[i] == ret) {
        inferer.eqn(this, Args[i]);
        break;
      }
    }
  } else {
    // Match this type to the concrete return type.
    inferer.ty(this, retTy);
  }
}
void Register::TypeInfer(TypeInferer &inferer) {
  inferer.eqn(this,Source);
}
void RegisterFunArg::TypeInfer(TypeInferer &inferer) {
  inferer.ty(this, ThisType); // Set in the Bind phase by func type signature.
}
void Func::TypeInfer(TypeInferer &inferer) {
  inferer.ty(Body, RetSType);
  Body->TypeInfer(inferer);
}
void Closure::TypeInfer(TypeInferer &inferer) {
  SFunctionType *ft = FuncRef->getFunctionSType();
  vector <SType*> funArgs = ft->getArgs();
  vector <SType*> closureArgs;

  for (unsigned i=ActivationRecord.size(), e=funArgs.size(); i != e; ++i)
    closureArgs.push_back(funArgs[i]);

  string Name("$SomeClosure$");
  inferer.ty(this, new SFunctionType(Name, closureArgs, ft->getReturnType()));

  vector<RegisterFunArg*> argRegs;
  FuncRef->getArgRegs(argRegs);
  for (unsigned i=0, e=ActivationRecord.size(); i != e; ++i)
    inferer.eqn(ActivationRecord[i], argRegs[i]);
}
void Array::TypeInfer(TypeInferer &inferer) {
  SType *ty = NamedTypes[STypeName];
  if (ty == NULL) {
    std::cerr << "Unknown type for Array: " << STypeName << std::endl;
    exit(1);
  }
  inferer.ty(this, new SArray(ty));
  Contained = ty;
}
void Constructor::TypeInfer(TypeInferer &inferer) {
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
  inferer.ty(this, sty);

  vector<SType*> argTys;
  sty->getElementSTypes(argTys);
  for (unsigned i=0, e=Args.size(); i != e; ++i) {
    Args[i]->TypeInfer(inferer);
    inferer.ty(Args[i], argTys[i]); // TODO: check for conflict in tys.
  }
}


unsigned TypeInferer::ResolveMembers() {
  vector<Member*> remainingMembers;
  for (unsigned i=0, e=members.size(); i != e; ++i) {
    Expr *src = members[i]->getSource();
    if (tys.count(src) > 0) {
      src->setSType(tys[src]);
      members[i]->TypeInferSecondPass();
      tys[members[i]] = members[i]->getSType();
    } else {
      remainingMembers.push_back(members[i]);
    }
  }

  unsigned membersResolved = members.size() - remainingMembers.size();
  if (membersResolved > 0)
    members.assign(remainingMembers.begin(), remainingMembers.end());
  return membersResolved;
}

unsigned TypeInferer::ResolveArrayAccesses() {
  vector<ArrayAccess*> remaining;
  for (unsigned i=0, e=arrayAccesses.size(); i != e; ++i) {
    Expr *src = arrayAccesses[i]->getSource();
    if (tys.count(src) > 0) {
      src->setSType(tys[src]);
      arrayAccesses[i]->TypeInferSecondPass();
      tys[arrayAccesses[i]] = arrayAccesses[i]->getSType();
    } else {
      remaining.push_back(arrayAccesses[i]);
    }
  }

  unsigned resolved = arrayAccesses.size() - remaining.size();
  if (resolved > 0)
    arrayAccesses.assign(remaining.begin(), remaining.end());
  return resolved;
}

void TypeInferer::TypeUnification() {
  std::list<pair<Expr*,Expr*> > es;
  {
    multimap<Expr*,Expr*>::const_iterator i;
    for (i=eqns.begin(); i != eqns.end(); i++) {
      es.push_back(*i);

      Expr *fst = i->first;
      if (fst->getSType() != NULL && tys.count(fst) == 0)
        tys[fst] = fst->getSType();

      Expr *snd = i->second;
      if (snd->getSType() != NULL && tys.count(snd) == 0)
        tys[snd] = snd->getSType();
    }
  }

  unsigned noMatchesIn = 0;
  while (es.size() > 0) {
    pair<Expr*,Expr*> eqn = es.front();
    es.pop_front();

    /*
    if (tys.count(eqn.first) > 0) {
      tys[eqn.second] = tys[eqn.first];
      noMatchesIn = 0;
    } else*/ if (tys.count(eqn.second) > 0) {
      if (tys.count(eqn.first) == 0)
        tys[eqn.first] = tys[eqn.second];
      noMatchesIn = 0;
    } else {
      es.push_back(eqn);
      noMatchesIn++;
    }

    if (noMatchesIn > es.size() + 2) {
      // Try resolving member accesses to give us more type fodder.
      if (ResolveMembers() == 0 && ResolveArrayAccesses() == 0) {
        // Could not resolve any members. Now we are in trouble.
        break;
      } else {
        noMatchesIn = 0;
      }
    }
  }

  while (ResolveMembers() > 0 || ResolveArrayAccesses() > 0) {
    ;
  }

  if (es.size() > 0) {
    std::vector<pair<Expr*,Expr*> > eqnsV(es.begin(), es.end()); // XXX for gdb
    std::cerr << "Unable to resolve all types." << std::endl;
    exit(1);
  } else if (members.size() > 0) {
    std::cerr << "Unable to resolve all type members." << std::endl;
    exit(1);
  } else if (arrayAccesses.size() > 0) {
    std::cerr << "Unable to resolve all array access types." << std::endl;
    exit(1);
  }
}

void TypeInferer::TypePopulation() {
  for (map<Expr*,SType*>::const_iterator i=tys.begin(); i!=tys.end(); i++)
    i->first->setSType(i->second);
}

void TypeInferer::eqn(Expr* lhs, Expr* rhs) {
  eqns.insert(pair<Expr*, Expr*>(lhs, rhs));
}
void TypeInferer::ty(Expr* expr, SType* ty) {
  tys.insert(pair<Expr*, SType*>(expr, ty));
}
void TypeInferer::member(Member *m) { members.push_back(m); }
void TypeInferer::arrayAccess(ArrayAccess *a) { arrayAccesses.push_back(a); }

}};
