/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.algorithm;


/**
 * Description of an element of a parameter for an algorithm.
 */
public class AlgorithmParameterElement {

  /** Text used to display the element */
  private final String text;

  /** Description of the element */
  private final String description;

  /** True if the parameter is optional */
  private final boolean optional;

  /**
   * Constructor.
   * 
   * @param text Text used to display the element.
   * @param description Description of the element.
   */
  public AlgorithmParameterElement(
      String text,
      String description) {
    this(text, description, false);
  }

  /**
   * Constructor.
   * 
   * @param text Text used to display the element.
   * @param description Description of the element.
   * @param optional True if the parameter is optional.
   */
  public AlgorithmParameterElement(
      String text,
      String description,
      boolean optional) {
    this.text = text;
    this.description = description;
    this.optional = optional;
  }

  /**
   * @return Text used to display the element.
   */
  public String getText() {
    return text;
  }

  /**
   * @return Description of the element.
   */
  public String getDescription() {
    return description;
  }

  /**
   * @return True if the parameter is optional.
   */
  public boolean isOptional() {
    return optional;
  }
}
