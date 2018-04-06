package jsy.student

import jsy.lab5.Lab5Like

object Lab5 extends jsy.util.JsyApplication with Lab5Like {
  import jsy.lab5.ast._
  import jsy.util.DoWith
  import jsy.util.DoWith._

  /*
   * CSCI 3155: Lab 5
   * Ian Smith
   *
   * Partner: <Your Partner's Name>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   *
   * Replace the '???' expression with your code in each function.
   *
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   *
   * Your lab will not be graded if it does not compile.
   *
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   */

  /*** Exercise with DoWith ***/

  def rename[W](env: Map[String,String], e: Expr)(fresh: String => DoWith[W,String]): DoWith[W,Expr] = {
    def ren(env: Map[String,String], e: Expr): DoWith[W,Expr] = e match {
      case N(_) | B(_) | Undefined | S(_) | Null | A(_) => doreturn(e)
      case Print(e1) => ren(env,e1) map { e1p => Print(e1p) }

      case Unary(uop, e1) => ren(env,e1) map { e1p => Unary(uop, e1p)}
      case Binary(bop, e1, e2) => ren(env,e1).flatMap({e1p => ren(env,e2).map(e2p => Binary(bop, e1p, e2p))})
      case If(e1, e2, e3) => ren(env, e1) flatMap { e1p => ren(env,e2) flatMap {e2p => ren(env,e3) map {e3p => If(e1p,e2p,e3p)}}}

      case Var(x) => doreturn(if (env contains x) Var(lookup(env,x)) else Var(x))

      case Decl(m, x, e1, e2) => fresh(x) flatMap { xp =>
        ren(env, e1) flatMap { e1p => ren(extend(env, x, xp),e2) map { e2p => Decl(m, xp, e1p, e2p)}}
      }

      case Function(p, params, retty, e1) => {
        val w: DoWith[W,(Option[String], Map[String,String])] = p match {
          case None => doreturn((None, env))
          case Some(x) => fresh(x) map ( xp => (Some(xp), env + (x -> xp)))
        }
        w flatMap { case (pp, envp) =>
          params.foldRight[DoWith[W,(List[(String,MTyp)],Map[String,String])]]( doreturn((Nil, envp)) ) {
            case ((x,mty), acc) => acc flatMap {
              case (ll, accenv) =>  fresh(x) map (xp => ((xp, mty) :: ll, accenv + (x -> xp))) // extend ll and map
            }
          } flatMap { // now sub in name
            case (paramsp, envpp) => ren(envpp, e1) map (e1p => Function(pp, paramsp, retty, e1p))
          }
        }
      }

      /*case Call(e1, args) => Call(ren(env,e1), args map {case (ei) => ren(env,ei)} )

      case Obj(fields) => Obj(fields mapValues( (ei) => ren(env,ei)))
      case GetField(e1, f) => GetField(ren(env, e1), f)*/

      case Call(e1, args) => ren(env, e1) flatMap( e1p => mapWith(args)(ei => ren(env, ei)) map (argsp => Call(e1p, argsp)))

      case Obj(fields) => mapWith(fields)(ei => ren(env,ei._2) map (exp => (ei._1, exp))) map (newfields => Obj(newfields)) // map field values
      case GetField(e1, f) => ren(env, e1) map (e1p => GetField(e1p, f))

      case Assign(e1, e2) => ren(env, e1) flatMap (e1p => ren(env,e2) map (e2p => Assign(e1p, e2p))) // just rename e1 and e2

      /* Should not match: should have been removed */
      case InterfaceDecl(_, _, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
    }
    ren(env, e)
  }

  def myuniquify(e: Expr): Expr = {
    val fresh: String => DoWith[Int,String] = { _ =>
      doget[Int] flatMap {(i) => val xp = "x" + i.toString; doput(i+1) map { _ => xp }} // (w) => (w,w) // still a problem here, int needs to be changed
    }
    val (_, r) = rename(empty, e)(fresh)(0)
    r
  }

  /*** Helper: mapFirst to DoWith ***/

  // List map with an operator returning a DoWith
  def mapWith[W,A,B](l: List[A])(f: A => DoWith[W,B]): DoWith[W,List[B]] = {
    l.foldRight[DoWith[W,List[B]]]( doreturn(Nil) ) { //
      case (a,dwbs) => dwbs flatMap {(bs:List[B]) => (f(a):DoWith[W,B]).map((b) => b :: bs)}// (w) => (w, f(a))
      // dwbs is our accumulator, flat map takes f : R => DoWith[W,R] , map takes f: R => B.
      // so we want to take our accumulated DoWith[W,List[B]] and map List[B] to List[f(a) :: List[B]]
    }
  }

  // Map map with an operator returning a DoWith
  def mapWith[W,A,B,C,D](m: Map[A,B])(f: ((A,B)) => DoWith[W,(C,D)]): DoWith[W,Map[C,D]] = {
   m.foldRight[DoWith[W,Map[C,D]]]( doreturn(Map()) ) {
      case (a, dwmap) => dwmap flatMap{ (currmap) => f(a) map {(cd) => currmap + (cd._1 -> cd._2)}}
    }
  }

  // Just like mapFirst from Lab 4 but uses a callback f that returns a DoWith in the Some case.
  def mapFirstWith[W,A](l: List[A])(f: A => Option[DoWith[W,A]]): DoWith[W,List[A]] = l match {
    case Nil => doreturn(l) //
    case h :: t => f(h) match {
      case None => mapFirstWith(t)(f) map { (ft) => h :: ft}
      case Some(fh) => fh map { (fh) => fh :: t}
    }
  }

  // There are better ways to deal with the combination of data structures like List, Map, and
  // DoWith, but we won't tackle that in this assignment.

  /*** Casting ***/

  def castOk(t1: Typ, t2: Typ): Boolean = (t1, t2) match {
      /***** Make sure to replace the case _ => ???. */
    //case _ => ???
    case (t11, t22) if t11 == t22 => true
    case (TNull, TObj(_)) => true // we can cast null to any obj
    case (TObj(tfields1), TObj(tfields2)) => // one must be subset of the other
      if (tfields2.toSet subsetOf tfields2.toSet) true // t2 is subset of t1
        else if (tfields1.toSet subsetOf tfields2.toSet) true // t1 is subset of t2
          else false
    //case _ => false // else false
      /***** Cases for the extra credit. Do not attempt until the rest of the assignment is complete. */
    case (TInterface(tvar, t1p), _) => ???
    case (_, TInterface(tvar, t2p)) => ???
      /***** Otherwise, false. */
    case _ => false
  }

  /*** Type Inference ***/

  // A helper function to check whether a jsy type has a function type in it.
  // While this is completely given, this function is worth studying to see
  // how library functions are used.
  def hasFunctionTyp(t: Typ): Boolean = t match {
    case TFunction(_, _) => true
    case TObj(fields) if (fields exists { case (_, t) => hasFunctionTyp(t) }) => true
    case _ => false
  }

  def isBindex(m: Mode, e: Expr): Boolean = m match {
    case (MVar| MConst |MName) => true
    case MRef => e match {
        // must be a location expression
      case Var(_) => true
      case GetField(_,_) => true
      case _ => false
    }
    case _ => false
  }

  def typeof(env: TEnv, e: Expr): Typ = {
    def err[T](tgot: Typ, e1: Expr): T = throw StaticTypeError(tgot, e1, e)

    e match {
      case Print(e1) => typeof(env, e1); TUndefined
      case N(_) => TNumber
      case B(_) => TBool
      case Undefined => TUndefined
      case S(_) => TString
      case Var(x) => val MTyp(m,t) = lookup(env, x); t
      case Unary(Neg, e1) => typeof(env, e1) match {
        case TNumber => TNumber
        case tgot => err(tgot, e1)
      }
        /***** Cases directly from Lab 4. We will minimize the test of these cases in Lab 5. */
      case Unary(Not, e1) => typeof(env, e1) match {
        case TBool => TBool
        case tgot => err(tgot, e1) // if we didn't get bool through error
      }
      case Binary(Plus, e1, e2) => (typeof(env,e1), typeof(env,e2)) match {
        case (TNumber,TNumber) => TNumber
        case (TString, TString) => TString
        case (TNumber|TString,tgot) => err(tgot, e2)
        case (tgot, TString|TNumber) => err(tgot,e1)
        case (tgot1, _) => err(tgot1, e1) // this is correct (trial and error)
      }
      case Binary(Minus|Times|Div, e1, e2) => (typeof(env ,e1), typeof(env,e2)) match {
        case (TNumber, TNumber) => TNumber
        case (tgot,TNumber) => err(tgot,e1)
        case (TNumber, tgot) => err(tgot, e2)
        case (tgot,_)=> err(tgot,e1)
      }
      case Binary(Eq|Ne, e1, e2) => (typeof(env,e1), typeof(env,e2)) match {
        case (t1, _) if hasFunctionTyp(t1) => err(t1,e1)
        case (_, t2) if hasFunctionTyp(t2) => err(t2, e2)
        case (t1, t2) => if (t1 == t2) TBool else err(t2, e2)
      }
      case Binary(Lt|Le|Gt|Ge, e1, e2) => (typeof(env,e1), typeof(env,e2)) match {
        case (TNumber,TNumber) => TBool
        case (TString, TString) => TBool
        case (TNumber|TString,tgot) => err(tgot, e2)
        case (tgot, TString|TNumber) => err(tgot,e1)
        case (tgot1, _) => err(tgot1, e1)
      }
      case Binary(And|Or, e1, e2) => (typeof(env,e1),typeof(env,e2)) match {
        case (TBool, TBool) => TBool
        case (TBool, tgot) => err(tgot, e2)
        case (tgot,_) => err(tgot, e1)
      }
      case Binary(Seq, e1, e2) => (typeof(env,e1), typeof(env,e2)) match {
        case (_ , t2) => t2
      }
      case If(e1, e2, e3) => (typeof(env,e1), typeof(env,e2), typeof(env,e3)) match {
        case (TBool, t1, t2)  => if(t1 == t2) t1 else err(t2,e2) // if it doesn't match the first
        case (tgot, _, _) => err(tgot, e1) // maybe not necessary
      }

      case Obj(fields) => fields foreach {(ei) => typeof(env,ei._2)}; TObj(fields mapValues { (ei) => typeof(env, ei)}) // catch error, else map

      case GetField(e1, f) =>  typeof(env,e1) match { // get type of e1
        case TObj(tfields) => tfields.get(f) match {// e1 must be an obj
          case Some(value) => value // type of that field
          case None => err(TObj(tfields), e1) // error
        }
        case tgot => err(tgot, e1) // anything besides object type
      }

        /***** Cases from Lab 4 that need a small amount of adapting. */
      case Decl(m, x, e1, e2) => val t1 = typeof(extend(env, x,  MTyp(m, typeof(env, e1))), e2)
          if (isBindex(m, e1)) t1 else err(t1, e1)
      case Function(p, params, tann, e1) => {
        // Bind to env1 an environment that extends env with an appropriate binding if
        // the function is potentially recursive.
        val env1 = (p, tann) match {
          case (Some(f), Some(tret)) =>
            val tprime = TFunction(params, tret)  // if function has a name, it can be recursive
            extend(env, f, MTyp(MConst,tprime))
          case (None, _) => env
          case _ => err(TUndefined, e1)
        }
        // Bind to env2 an environment that extends env1 with bindings for params.
        val env2 = params.foldLeft(env1)({ case (currEnv : TEnv, (s :String, mt @ MTyp(_,_))) =>currEnv + (s -> mt)}) // extend env by each parameter
        val t1 = typeof(env2, e1)
        // Match on whether the return type is specified.
        tann match {
          case None => TFunction(params, t1)
          case Some(tret) => if(TFunction(params,t1) == TFunction(params,tret)) TFunction(params, t1) else err(TFunction(params, tret), e1)
        }
      }

      case Call(e1, args) => typeof(env, e1) match {
        case TFunction(params, tret) if (params.length == args.length) => {
          (params zip args).foreach {
            case ((_, MTyp(_, t)), arg) => if (t != typeof(env, arg)) err(typeof(env, arg), arg) // check that they are equal types (otherwise there is an error)
          }
          tret
        }
        case tgot => err(tgot, e1)
      }
      case Call(e1, args) => typeof(env, e1) match {
        case TFunction(params, tret) if (params.length == args.length) =>
          (params, args).zipped.foreach {
            // check that correct type and that it is bindable
            case ((s, MTyp(m,t)), arg) => if ((t != typeof(env, arg)) || (!isBindex(m, arg))) err(typeof(env, arg), arg)
          }
          tret // this is the type if there were no type errors
        case tgot => err(tgot, e1)
      }

        /***** New cases for Lab 5. ***/
      case Assign(Var(x), e1) => lookup(env, x) match {
        case MTyp(m,t) => m match {
          case (MVar | MRef) => typeof(env, e1) match {
            case tgot => if (tgot == t) t else err(tgot, e1)
          }
          case _ => ???
        }
        case _ => ???
      }
      case Assign(GetField(e1, f), e2) => typeof(env, e1) match {
        case TObj(fields) => val t1 = lookup(fields, f); if (t1 == typeof(env,e2)) t1 else err(t1, e1)
        case tgot => err(tgot, e1)
      }


      case Assign(_, _) => err(TUndefined, e)

      case Null =>
        TNull // base case

      case Unary(Cast(t), e1) => typeof(env, e1) match {
        case tgot if castOk(tgot, t) => t
        case tgot => err(tgot, e1)
      }

      /* Should not match: non-source expressions or should have been removed */
      case A(_) | Unary(Deref, _) | InterfaceDecl(_, _, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
    }
  }

  /*** Small-Step Interpreter ***/

  /*
   * Helper function that implements the semantics of inequality
   * operators Lt, Le, Gt, and Ge on values.
   *
   * We suggest a refactoring of code from Lab 2 to be able to
   * use this helper function in eval and step.
   *
   * This should the same code as from Lab 3 and Lab 4.
   */
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(isValue(v1), s"inequalityVal: v1 ${v1} is not a value")
    require(isValue(v2), s"inequalityVal: v2 ${v2} is not a value")
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    // COPIED FROM LAB 3 (removed toNumber)
    (v1, v2) match {
      case (S(s1), S(s2)) => bop match { // must be string or number
        case Lt => s1<s2
        case Le => s1<=s2
        case Gt => s1>s2
        case Ge => s1>=s2
      }
      case (N(n1), N(n2)) => bop match {
        case Lt => n1 < n2
        case Le => n1 <= n2
        case Gt => n1 > n2
        case Ge => n1 >= n2
      }
    }
  }

  /* Capture-avoiding substitution in e replacing variables x with esub. */
  def substitute(e: Expr, esub: Expr, x: String): Expr = {
    def subst(e: Expr): Expr = e match {
      case N(_) | B(_) | Undefined | S(_) | Null | A(_) => e
      case Print(e1) => Print(subst(e1))
        /***** Cases from Lab 3 */
      case Unary(uop, e1) => Unary(uop, subst(e1)) // sub v for x in inner expressions
      case Binary(bop, e1, e2) => Binary(bop, subst(e1), subst(e2)) // sub inner expressions
      case If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3))
      case Var(y) => if (x == y) esub else Var(y)
        /***** Cases need a small adaption from Lab 3 */
      case Decl(mut, y, e1, e2) => Decl(mut, y, subst(e1), if (x == y) e2 else subst(e2))
        /***** Cases needing adapting from Lab 4 */
      case Function(p, params, retty, e1) => p match {
        case Some(pp) => if (pp == x || params.exists(pa => pa._1 == x)) e else Function(p, params, retty, subst(e1))
        case None => if (params.exists(pa => pa._1 == x)) e else Function(p, params, retty, subst(e1))
      }
        /***** Cases directly from Lab 4 */
      case Call(e1, args) => Call(subst(e1), args map {ei => subst(ei)}) // substitute in for all the args
      /***** New cases for Lab 4 */
      case Obj(fields) => Obj(fields mapValues { (exp) => subst(exp)}) // substitute all x in all value expr with esub
      case GetField(e1, f) => GetField(subst(e1), f)
        /***** New case for Lab 5 */
      case Assign(e1, e2) => Assign(subst(e1), subst(e2))

      /* Should not match: should have been removed */
      case InterfaceDecl(_, _, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
    }

    def myrename(e: Expr): Expr = {
      val fvs = freeVars(esub)
      def fresh(x: String): String = if (fvs contains x) fresh(x + "$") else x
      rename[Unit](e)(0){ x => doreturn(fresh(x)) }
    }

    subst(myrename(e))
  }

  /* Check whether or not an expression is reduced enough to be applied given a mode. */
  def isRedex(mode: Mode, e: Expr): Boolean = mode match {
    case (MConst|MVar) if !isValue(e) => true
    case MRef => e match {
      case lv if isLValue(lv) => false
      case _ => true
    }
    case _ => false
  }

  def getBinding(mode: Mode, e: Expr): DoWith[Mem,Expr] = {
    require(!isRedex(mode,e), s"expression ${e} must not reducible under mode ${mode}")
    mode match {
      case MConst => doreturn(e)
      case MName => doreturn(e)
      case MRef => doreturn(e)
      case MVar => memalloc(e) map ( addr => Unary(Deref, addr))
    }
  }

  /* A small-step transition. */
  def step(e: Expr): DoWith[Mem, Expr] = {
    require(!isValue(e), "stepping on a value: %s".format(e))
    e match {
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => doget map { m => println(pretty(m, v1)); Undefined }
        /***** Cases needing adapting from Lab 3. */
      /*case Unary(Neg, v1) if isValue(v1) => doget map { _ =>
        v1 match {
          case N(n1) => N(-n1)
          case _ => throw StuckError(e)
        }
      }*/
      case Unary(Neg, v1) if isValue(v1) => v1 match {
        case N(n1) => doreturn( N(-n1))
        case _ => throw StuckError(e)
      }
      case Unary(Not, v1) if isValue(v1) => doget map { _ =>
        v1 match {
          case B(b1) => B(!b1)
          case _ => throw StuckError(e)
        }
      }
      case Binary(Seq, v1, e2) if isValue(v1) => doget map { _ => e2} // do seq

      case Binary(Plus, v1, v2) if isValue(v1) && isValue(v2) => doget map { _ =>
        (v1, v2) match { // do plus
          case (S(s1), S(s2)) => S(s1 + s2)
          case (N(n1), N(n2)) => N(n1 + n2)
          case _ => throw StuckError(e)
        }
      }
      case Binary(bop @ (Eq | Ne), v1,v2) if isValue(v1) && isValue(v2) => doget map { _ =>
        bop match {
          case Eq => B(v1 == v2)
          case Ne => B(v1 != v2)
        }
      }
      // these binary cases need to come before the general bop case
      case Binary(And, v1, e2) if isValue(v1) => doget map { _ =>
        v1 match {
          case B(b) => if (b) e2 else B(false)
          case _ => throw StuckError(e)
        }
      }
      // match on And | Or
      case Binary(Or, v1, e2) if isValue(v1) => doget map { _ =>
        v1 match {
          case B(b) => if (b) B(true) else e2
          case _ => throw StuckError(e)
        }
      }
      case Binary(bop, v1, v2) if isValue(v1) && isValue(v2) => doget map { _ =>
        (v1, v2) match { // do arith
          case (N(n1), N(n2)) => bop match {
            case Minus => N(n1 - n2) // OMG I HAD N1+N2 instead of n1-n2; this mistake took me literally 3 hours to debug
            case Div => N(n1 / n2)
            case Times => N(n1 * n2)
            case Lt | Le | Gt | Ge => B(inequalityVal(bop, v1, v2)) // do inequality (all cases handled by inequalityVal() )
          }
          case (S(_), S(_)) => bop match {
            case Lt | Le | Gt | Ge => B(inequalityVal(bop, v1, v2)) // case for strings
            case _ => throw StuckError(e) // we can only add or compare strings
          }
          case _ => throw StuckError(e)
        }
      }
      case If(v1, e2, e3) if isValue(v1) => doget map { _ =>
        v1 match {
          // DoIfTrue and DoIfFalse
          case B(b) => if (b) e2 else e3
          case _ => throw StuckError(e)
        }
      }
        /***** More cases here */
        /***** Cases needing adapting from Lab 4. */
      case Obj(fields) if (fields forall { case (_, vi) => isValue(vi)}) =>
        memalloc(e) map { a => a}
      case GetField(a @ A(_), f) => doget map {m => m(a) match {
        case Obj(fields) => fields(f)
        case _=> throw StuckError(e)
        }
      }

      case Decl(m, x, e1, e2) if !isRedex(m, e1) => getBinding(m, e1) map { e1p => substitute(e2, e1p, x)}
      /*case Decl(MConst, x, v1, e2) if isValue(v1) =>
        doreturn(substitute(e2, v1, x)) // for MConst, <M, v> -> <M,v>
      case Decl(MVar, x, v1, e2) if isValue(v1) =>
        memalloc(v1) map ( addr => substitute(e2, Unary(Deref, addr), x)) // this comes from VarBind*/

        /***** New cases for Lab 5. */
      case Unary(Deref, a @ A(_)) =>
        doget map {addr => addr get a match {
          case Some(expr) => expr
          case None => throw StuckError(e)
          }
        }
      case Unary(Cast(t), Null) => t match {
        case TObj(_) => doreturn(Null)
        case _=> throw StuckError(e)
      }

      case Unary(Cast(t), v1) if isValue(v1) => v1 match {
        case a @ A(_) => doget map { addr => addr get a match {
          case Some(Obj(fields)) => t match {
            case TObj(tfields) => if (tfields forall { case (fi:String, ti : Typ) => fields contains fi }) a else throw DynamicTypeError(e)
            case _ => throw StuckError(e)
          }
          case _ => throw StuckError(e)
          }
        }
        case _ => doreturn(v1)
      }
      case Assign(Unary(Deref, a @ A(_)), v) if isValue(v) =>
        domodify[Mem] { m => m + (a -> v) } map { _ => v } // update memory and then change result

      case Assign(GetField(a @ A(_), f), v) if isValue(v) =>
        domodify[Mem] {
          m => m(a) match {
            // it must be an object otherwise we can't do the getfield and assign
            case Obj(fields) => m + (a -> Obj(fields + ((f,v))))  // update map with pointing a to this updated object
            case _=> throw StuckError(e)
          }
        } map { _ => v } // update result to v

      case Call(v @ Function(p, params, _, e), args) => { // is Function
        val pazip = params zip args
        if (pazip forall{  case ((_, MTyp(m,_)), arg) => !isRedex(m, arg)}) {
          val dwep = pazip.foldRight( doreturn(e) : DoWith[Mem,Expr] )  {
            case (((xi, MTyp(mi, _)), ei), dwacc) => getBinding(mi, ei) flatMap { eip => dwacc map { ep => substitute(ep, eip, xi)}}
          }
          p match {
            case None => dwep
            case Some(x) => dwep map { epp => substitute(epp, v, x)}
          }
        }
        else {
          val dwpazipp = mapFirstWith(pazip) {
            case (param@(_: String, MTyp(m, _)), arg: Expr) if isRedex(m, arg) => Some(step(arg) map {argp => (param, argp)}) // map first reducible arg
            case _ => None
          }
          dwpazipp map {pa => Call(v,pa.unzip._2)} // searchCall2 with updated args
        }
      }

      /* Base Cases: Error Rules */
        /***** Replace the following case with a case to throw NullDeferenceError.  */
      case GetField(Null, _) => throw NullDereferenceError(e)
      case Assign(GetField(Null, _), e2) => throw NullDereferenceError(e)

      /* Inductive Cases: Search Rules */
        /***** Cases needing adapting from Lab 3. Make sure to replace the case _ => ???. */
      case Print(e1) => step(e1) map { e1p => Print(e1p) }
      case Unary(uop, e1) =>
        step (e1) map {e1p => Unary(uop , e1p)}

      case Binary(bop, v1, e2) if isValue(v1) => step(e2) map { e2p => Binary(bop, v1, e2p)} // v1 is value BinarySearch 2
      case Binary(bop, e1, e2) => step(e1) map {e1p => Binary(bop, e1p, e2)}
      case If(e1, e2, e3) => step(e1) map {e1p => If(e1p, e2, e3)} // e1 is not a value (wouldve been caught earlier)

        /***** Cases needing adapting from Lab 4 */
      // Search GetField
      case GetField(e1, f) =>
        step(e1) map {e1p => GetField(e1p, f)}
      // SearchObj
      case Obj(fields) =>
        fields find {f => !isValue(f._2)} match { // find first key that doesn't map to value
          case Some((fi,ei)) => step(ei) map {eip => Obj(fields + (fi -> eip))}
        }
        // search Decl
      case Decl(mode, x, e1, e2) => step(e1) map {e1p => Decl(mode, x, e1p, e2)} // is Redex == true
      // searchCall1
      case Call(e1, args) =>
        step(e1) map {e1p => Call(e1p, args)} // step e1 if not Function

        /***** New cases for Lab 5.  */
      case Assign(e1, e2) if ??? =>
        ???
      case Assign(e1, e2) =>
        ???

      /* Everything else is a stuck error. */
      case _ => throw StuckError(e)
    }
  }

  /*** Extra Credit: Lowering: Remove Interface Declarations ***/

  def lower(e: Expr): Expr =
    /* Do nothing by default. Change to attempt extra credit. */
    e

  /*** External Interfaces ***/

  //this.debug = true // comment this out or set to false if you don't want print debugging information
  this.maxSteps = Some(1000) // comment this out or set to None to not bound the number of steps.
  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file
}
