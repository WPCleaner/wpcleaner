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

import org.wikipediacleaner.api.constants.WikiConfiguration;


/**
 * Bean representing a Linter error.
 */
public class LinterError {

  /** Error type */
  private final String type;

  /** Parameters */
  private final Map<String, String> parameters;

  /** Start offset of the error */
  private final int startOffset;

  /** End offset of the error */
  private final int endOffset;

  /**
   * @param type Error type.
   * @param parameters Parameters.
   * @param startOffset Start of the error.
   * @param endOffset End of the error.
   */
  public LinterError(
      String type,
      Map<String, String> parameters,
      int startOffset,
      int endOffset) {
    this.type = type;
    this.parameters = (parameters != null) ? new HashMap<String, String>(parameters) : null;
    this.startOffset = startOffset;
    this.endOffset = endOffset;
  }

  /**
   * @return Error type.
   */
  public String getType() {
    return type;
  }

  /**
   * @return Error type.
   */
  public String getTypeName(WikiConfiguration config) {
    if (config != null) {
      String result = config.getMessageByName("linter-category-" + type);
      if (result != null) {
        return result;
      }
    }
    return type;
  }

  /**
   * @return Parameters.
   */
  public Map<String, String> getParameters() {
    return (parameters != null) ? Collections.unmodifiableMap(parameters) : null;
  }

  /**
   * @return Start offset of the error.
   */
  public int getStartOffset() {
    return startOffset;
  }

  /**
   * @return End offset of the error.
   */
  public int getEndOffset() {
    return endOffset;
  }
}
