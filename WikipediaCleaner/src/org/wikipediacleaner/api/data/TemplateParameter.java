/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;


/**
 * Utility class for memorizing template parameters.
 */
public class TemplateParameter {

  private String name;
  private String value;
  private boolean relevant;

  /**
   * @param name Parameter name.
   * @param value Parameter value.
   */
  public TemplateParameter(String name, String value) {
    this.name = name;
    this.value = value;
    this.relevant = false;
  }

  /**
   * @return Parameter name.
   */
  public String getName() {
    return name;
  }

  /**
   * @return Parameter value.
   */
  public String getValue() {
    return value;
  }

  /**
   * @return Flag indicating if the parameter is relevant.
   */
  public boolean isRelevant() {
    return relevant;
  }

  /**
   * @param relevant Flag indicating if the parameter is relevant.
   */
  public void setRelevant(boolean relevant) {
    this.relevant = relevant;
  }
}
