/* Copyright 2009-2016 EPFL, Lausanne */

package leon.codegen.runtime;

public abstract class Lambda {
  public abstract Object apply(Object[] args) throws LeonCodeGenRuntimeException;
  public abstract void checkForall(boolean[] quantified);
  public abstract void checkAxiom();
}
