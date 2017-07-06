/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2017  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.linter;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;


/**
 * Bean representing a Linter error.
 */
public class LinterError {

  /** Error type */
  private final String type;

  /** Parameters */
  private final Map<String, String> parameters;

  /** Start position of the error */
  private final int start;

  /** End position of the error */
  private final int end;

  /**
   * @param type Error type.
   * @param parameters Parameters.
   * @param start Start of the error.
   * @param end End of the error.
   */
  public LinterError(
      String type,
      Map<String, String> parameters,
      int start,
      int end) {
    this.type = type;
    this.parameters = (parameters != null) ? new HashMap<String, String>(parameters) : null;
    this.start = start;
    this.end = end;
  }

  /**
   * @return Error type.
   */
  public String getType() {
    return type;
  }

  /**
   * @return Parameters.
   */
  public Map<String, String> getParameters() {
    return (parameters != null) ? Collections.unmodifiableMap(parameters) : null;
  }

  /**
   * @return Start index of the error.
   */
  public int getStart() {
    return start;
  }

  /**
   * @return End index of the error.
   */
  public int getEnd() {
    return end;
  }
}
