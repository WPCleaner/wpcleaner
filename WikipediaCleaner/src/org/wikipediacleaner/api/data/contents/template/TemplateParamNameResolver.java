/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.data.contents.template;


/**
 * A utility class to resolve parameter name in a template.
 */
public class TemplateParamNameResolver {

  private Integer nextParamNum = Integer.valueOf(1);

  /**
   * Default constructor.
   */
  public TemplateParamNameResolver() {
  }

  /**
   * Compute next parameter name, but do not register it.
   * 
   * @param parameterName Actual parameter name.
   * @return Computed parameter name.
   */
  public String nextParameterWithoutRegister(String parameterName) {
    String result = parameterName;
    if ((result == null) || (result.length() == 0)) {
      result = nextParamNum.toString();
    }
    return result;
  }

  /**
   * Register next parameter and compute its name.
   * 
   * @param parameterName Actual parameter name.
   * @return Computed parameter name.
   */
  public String nextParameter(String parameterName) {
    String result = nextParameterWithoutRegister(parameterName);
    if (result.equals(nextParamNum.toString())) {
      nextParamNum = Integer.valueOf(nextParamNum.intValue() + 1);
    }
    return result;
  }

  /**
   * @return Name that will be used for the next unnamed parameter.
   */
  public String getNextParameter() {
    return nextParamNum.toString();
  }
}
