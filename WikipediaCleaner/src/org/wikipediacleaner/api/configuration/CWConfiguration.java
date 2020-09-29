/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.configuration;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import org.wikipediacleaner.api.APIException;
import org.wikipediacleaner.api.algorithm.AlgorithmError;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.Configuration;
import org.wikipediacleaner.utils.ConfigurationValueBoolean;


/**
 * Configuration for Check Wiki project.
 */
public class CWConfiguration {

  /**
   * Maximum number for error numbers.
   */
  public static final int MAX_ERROR_NUMBER = 999;

  /**
   * Check Wiki project code.
   */
  private final String code;

  /**
   * Wiki.
   */
  private final EnumWikipedia wiki;

  public CWConfiguration(String code, EnumWikipedia wiki) {
    this.code = code;
    this.wiki = wiki;
    this.configuration = new ArrayList<CWConfigurationError>();
  }

  // =================================================================================
  // Project availability
  // =================================================================================

  /**
   * @return Flag indicating if the Check Wiki project is available.
   */
  public boolean isProjectAvailable() {
    WPCConfiguration config = wiki.getConfiguration();
    String project = config.getString(WPCConfigurationString.CW_PROJECT_PAGE);
    if (project != null) {
      return true;
    }
    return config.getBoolean(WPCConfigurationBoolean.CW_FORCE);
  }

  // =================================================================================
  // Comment
  // =================================================================================

  /**
   * @return Comment for Check Wiki project.
   */
  public String getComment() {
    WPCConfiguration config = wiki.getConfiguration();
    String comment = config.getString(WPCConfigurationString.CW_COMMENT);
    if (comment != null) {
      return comment;
    }
    String project = config.getString(WPCConfigurationString.CW_PROJECT_PAGE);
    return GT._T("Fixed using {0}", (project != null) ? "[[" + project + "]]" : "Check Wiki project");
  }

  /**
   * @param algorithms Algorithms.
   * @return Comment for Check Wiki project.
   */
  public String getComment(Collection<AlgorithmError.Progress> algorithms) {
    if ((algorithms == null) || (algorithms.isEmpty())) {
      return "";
    }
    
    // Build part of the comment for the errors that were fixed
    StringBuilder algorithmsComment = new StringBuilder();
    Configuration config = Configuration.getConfiguration();
    Set<String> textsAdded = new HashSet<>();
    for (AlgorithmError.Progress progress : algorithms) {

      // Compute text for the algorithm
      String text = null;
      CheckErrorAlgorithm algorithm = progress.algorithm;
      String link = algorithm.getLink();
      if ((link != null) &&
          (config != null) &&
          (config.getBoolean(
              null,
              ConfigurationValueBoolean.CHECK_LINK_ERRORS))) {
        text = "[[" + link + "|" + algorithm.getShortDescriptionReplaced() + "]]";
      } else {
        text = algorithm.getShortDescriptionReplaced();
      }

      // Add text
      if (!textsAdded.contains(text)) {
        if (algorithmsComment.length() > 0) {
          algorithmsComment.append(" - ");
        }
        algorithmsComment.append(text);
        textsAdded.add(text);
      }
    }

    // Build the entire comment
    String comment = getComment();
    if (comment.indexOf("{0}") < 0) {
      return comment + " (" + algorithmsComment.toString() + ")";
    }
    return GT._T(comment, algorithmsComment.toString());
  }

  // =================================================================================
  // Error configuration
  // =================================================================================

  /**
   * Configuration for each error.
   */
  private final ArrayList<CWConfigurationError> configuration;

  /**
   * @param errorNumber Error number.
   * @return Configuration for error.
   */
  public CWConfigurationError getErrorConfiguration(int errorNumber) {
    if ((errorNumber < 0) || (errorNumber >= configuration.size())) {
      return null;
    }
    return configuration.get(errorNumber);
  }

  /* ================================================================================= */
  /* Property management                                                               */
  /* ================================================================================= */

  /**
   * Prefix for property names.
   */
  private final static String PREFIX = "error_";

  /**
   * Enumerated value for the origin of the configuration.
   */
  public static enum Origin {
    GENERAL_CONFIGURATION,
    WIKI_CONFIGURATION,
    USER_CONFIGURATION,
  }

  /**
   * Set property.
   * 
   * @param name Property name.
   * @param value Property value.
   * @param suffix Suffix for properties to consider.
   * @param otherSuffix Suffix for properties not to consider.
   * @param origin Origin of the configuration.
   */
  private void setProperty(
      String name, String value,
      String suffix, String otherSuffix,
      Origin origin) {
    boolean matches = true;

    // Check that property prefix is OK
    if (matches && name.startsWith(PREFIX)) {
      name = name.substring(PREFIX.length());
    } else {
      matches = false;
    }

    // Check that property suffix is OK
    boolean goodSuffix = false;
    if (matches && name.endsWith(suffix)) {
      name = name.substring(0, name.length() - suffix.length());
      goodSuffix = true;
    } else if (matches && name.endsWith(otherSuffix)) {
      name = name.substring(0, name.length() - otherSuffix.length());
    } else {
      matches = false;
    }
    if (matches && name.endsWith("_")) {
      name = name.substring(0, name.length() - 1);
    } else {
      matches = false;
    }

    // Retrieve error number
    int errorNumber = -1;
    if (matches && (name.length() > 4) &&
        Character.isDigit(name.charAt(0)) &&
        Character.isDigit(name.charAt(1)) &&
        Character.isDigit(name.charAt(2)) &&
        (name.charAt(3) == '_')) {
      try {
        errorNumber = Integer.parseInt(name.substring(0, 3));
      } catch (NumberFormatException e) {
        // Not supposed to happen
      }
      name = name.substring(4);
    } else {
      matches = false;
    }

    // Save property
    if (matches && (errorNumber > 0) && (errorNumber <= MAX_ERROR_NUMBER)) {
      if (goodSuffix) {
        CWConfigurationError error = null;
        if (configuration.size() > errorNumber) {
          error = configuration.get(errorNumber);
        }
        if (error == null) {
          error = new CWConfigurationError(errorNumber);
          configuration.ensureCapacity(errorNumber + 1);
          while (configuration.size() < errorNumber + 1) {
            configuration.add(configuration.size(), null);
          }
          configuration.set(errorNumber, error);
        }
        error.addProperty(name, value, origin);
      }
    }
  }

  /**
   * Extract next parameter from Check Wiki configuration.
   *
   * @param reader Reader for the configuration.
   * @param suffix Suffix for properties to consider.
   * @param otherSuffix Suffix for properties not to consider.
   * @param origin Origin of the configuration.
   * @return Next parameter found.
   * @throw APIException.
   */
  private boolean setNextParameter(
      BufferedReader reader,
      String suffix, String otherSuffix,
      Origin origin) throws APIException {
    String line;
    try {
      while ((line = reader.readLine()) != null) {
        int posEqual = line.indexOf('=');
        int posSharp = line.indexOf('#');
        if ((posEqual > 0) &&
            ((posSharp < 0) || (posSharp > posEqual))) {
          String name = line.substring(0, posEqual);
          line = line.substring(posEqual + 1);
          int posEnd = line.indexOf(" END");
          while (posEnd == -1) {
            String nextLine = reader.readLine();
            if (nextLine != null) {
              line += "\n" + nextLine;
              posEnd = line.indexOf(" END");
            } else {
              posEnd = line.length();
            }
          }
          line = line.substring(0, posEnd);
          if ((name != null) && (line != null)) {
            setProperty(name.trim(), line, suffix, otherSuffix, origin);
          }
          return true;
        }
      }
    } catch (IOException e) {
      throw new APIException("Error reading Check Wiki configuration: " + e.getMessage());
    }
    return false;
  }

  /**
   * @param input Reader for general project configuration.
   * @throws APIException Exception thrown by the API.
   */
  public void setGeneralConfiguration(Reader input) throws APIException {
    BufferedReader reader = new BufferedReader(input);
    while (setNextParameter(reader, "script", code, Origin.GENERAL_CONFIGURATION)) {
      //
    }
    try {
      reader.close();
    } catch (IOException e) {
      // Nothing
    }
  }

  /**
   * @param input Reader for project configuration specific for this wiki.
   * @throws APIException Exception thrown by the API.
   */
  public void setWikiConfiguration(Reader input) throws APIException {
    BufferedReader reader = new BufferedReader(input);
    while (setNextParameter(reader, code, "script", Origin.WIKI_CONFIGURATION)) {
      //
    }
    try {
      reader.close();
    } catch (IOException e) {
      // Nothing
    }
  }

  /**
   * @param name Parameter name.
   * @param value Parameter value.
   */
  void setUserConfiguration(String name, String value) {
    setProperty(name, value, code, "script", Origin.USER_CONFIGURATION);
  }
}
