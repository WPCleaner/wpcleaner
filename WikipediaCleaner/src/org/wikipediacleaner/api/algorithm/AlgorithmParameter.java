/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.algorithm;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Bean for describing parameters used by algorithms.
 */
public class AlgorithmParameter implements Comparable<AlgorithmParameter> {

  /** Name of the parameter */
  private final String name;

  /** Textual description of the parameter */
  private final String description;

  /** List of elements of the parameter */
  private final List<AlgorithmParameterElement> elements = new ArrayList<>();

  /** True if the parameter has multiple lines */
  private final boolean multiline;

  /**
   * Constructor.
   * 
   * @param name Name of the parameter.
   * @param description Textual description of the parameter.
   */
  public AlgorithmParameter(
      String name,
      String description) {
    this(name, description, (List<AlgorithmParameterElement>) null, false);
  }

  /**
   * Constructor.
   * 
   * @param name Name of the parameter.
   * @param description Textual description of the parameter.
   * @param element Single element of the parameter.
   */
  public AlgorithmParameter(
      String name,
      String description,
      AlgorithmParameterElement element) {
    this(name, description, element, false);
  }

  /**
   * Constructor.
   * 
   * @param name Name of the parameter.
   * @param description Textual description of the parameter.
   * @param element Single element of the parameter.
   * @param multiline True if the parameter has multiple lines.
   */
  public AlgorithmParameter(
      String name,
      String description,
      AlgorithmParameterElement element,
      boolean multiline) {
    this(name, description, Collections.singletonList(element), multiline);
  }

  /**
   * Constructor.
   * 
   * @param name Name of the parameter.
   * @param description Textual description of the parameter.
   * @param elements Elements of the parameter.
   * @param multiline True if the parameter has multiple lines.
   */
  public AlgorithmParameter(
      String name,
      String description,
      List<AlgorithmParameterElement> elements,
      boolean multiline) {
    this.name = name;
    this.description = description;
    if (elements != null) {
      this.elements.addAll(elements);
    }
    this.multiline = multiline;
  }

  /**
   * Constructor.
   * 
   * @param name Name of the parameter.
   * @param description Textual description of the parameter.
   * @param elements Elements of the parameter.
   * @param multiline True if the parameter has multiple lines.
   */
  public AlgorithmParameter(
      String name,
      String description,
      AlgorithmParameterElement[] elements,
      boolean multiline) {
    this.name = name;
    this.description = description;
    if (elements != null) {
      for (AlgorithmParameterElement element : elements) {
        this.elements.add(element);
      }
    }
    this.multiline = multiline;
  }

  /**
   * @return Name of the parameter.
   */
  public String getName() {
    return name;
  }

  /**
   * @return Textual description of the parameter.
   */
  public String getDescription() {
    return description;
  }

  /**
   * @return Elements of the algorithm.
   */
  public List<AlgorithmParameterElement> getElements() {
    return elements;
  }

  /**
   * @return A line of text representing all the elements of the parameter.
   */
  public String getElementsLine() {
    StringBuilder buffer = new StringBuilder();
    int optionalCount = 0;
    for (AlgorithmParameterElement element : getElements()) {
      boolean first = (buffer.length() == 0);
      if (element.isOptional()) {
        buffer.append("[");
        optionalCount++;
      }
      if (!first) {
        buffer.append("|");
      }
      buffer.append(element.getText());
      if (element.canBeMultiple()) {
        buffer.append("...");
      }
    }
    for (int i = 0; i < optionalCount; i++) {
      buffer.append("]");
    }
    return buffer.toString();
  }

  /**
   * @return True if parameter can span multiple lines.
   */
  public boolean isMultiLine() {
    return multiline;
  }

  /**
   * Compare to algorithm parameters.
   * 
   * @param o the algorithm parameter to be compared.
   * @return a negative integer, zero, or a positive integer as this algorithm parameter
   *         is less than, equal to, or greater than the specified algorithm parameter.
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  @Override
  public final int compareTo(AlgorithmParameter o) {
    return name.compareTo(o.name);
  }
}
