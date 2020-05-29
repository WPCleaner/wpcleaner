/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2020  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */


package org.wikipediacleaner.api.algorithm;

import java.util.ArrayList;
import java.util.List;

/**
 * A utility class to generate a description of algorithm parameters.
 */
public class AlgorithmParametersDescriptor {

  /** Global prefix to use for a description */
  private String globalPrefix;

  /** Global suffix to use for a description */
  private String globalSuffix;

  /** Parameter prefix to use for a description */
  private String parameterPrefix;

  /** Parameter suffix to use for a description */
  private String parameterSuffix;

  /** Parameter name prefix to use for a description */
  private String namePrefix;

  /** Parameter name suffix to use for a description */
  private String nameSuffix;

  /** Parameter description prefix to use for a description */
  private String descriptionPrefix;

  /** Parameter description suffix to use for a description */
  private String descriptionSuffix;

  /** True if details of each parameter should be displayed */
  private boolean displayDetails;

  /** Details prefix to use for a description */
  private String detailsPrefix;

  /** Details suffix to use for a description */
  private String detailsSuffix;

  /** Prefix to use for parameter name in the example */
  private String examplePrefix;

  /** Suffix to use for parameter name in the example */
  private String exampleSuffix;

  /** True if tooltip should be displayed */
  private boolean displayTooltip;

  /** Begin of prefix to use for a tooltip */
  private String tooltipPrefixBegin;

  /** End of prefix to use for a tooltip */
  private String tooltipPrefixEnd;

  /** Suffix to use for a tooltip */
  private String tooltipSuffix;

  /** Text to use for line separation */
  private String lineSeparation;

  /** Text to use for line tabulation */
  private String lineTabulation;

  /**
   * Constructor.
   * 
   * Configuration is done through the set methods.
   */
  public AlgorithmParametersDescriptor() {
    lineSeparation = "\n";
    lineTabulation = "  ";
  }

  /**
   * Configure global texts to use for the description.
   * 
   * @param prefix Text before the global description.
   * @param suffix Text after the global description.
   */
  public void setGlobalTexts(String prefix, String suffix) {
    this.globalPrefix = prefix;
    this.globalSuffix = suffix;
  }

  /**
   * Configure parameter texts to use for the description.
   * 
   * @param prefix Text before each parameter.
   * @param suffix Text after each parameter.
   */
  public void setParameterTexts(String prefix, String suffix) {
    this.parameterPrefix = prefix;
    this.parameterSuffix = suffix;
  }

  /**
   * Configure parameter name texts to use for the description.
   * 
   * @param prefix Text before each parameter name.
   * @param suffix Text after each parameter name.
   */
  public void setParameterNameTexts(String prefix, String suffix) {
    this.namePrefix = prefix;
    this.nameSuffix = suffix;
  }

  /**
   * Configure parameter description texts to use for the description.
   * 
   * @param prefix Text before each parameter description.
   * @param suffix Text after each parameter description.
   */
  public void setParameterDescriptionTexts(String prefix, String suffix) {
    this.descriptionPrefix = prefix;
    this.descriptionSuffix = suffix;
  }

  /**
   * Configure the display of details about each parameter.
   * 
   * @param display True to display the details about each parameter.
   */
  public void setDisplayDetails(boolean display) {
    this.displayDetails = display;
  }

  /**
   * Configure texts to use for the details.
   * 
   * @param prefix Text before each details about a parameter.
   * @param suffix Text after each details about a parameter.
   */
  public void setDetailsTexts(String prefix, String suffix) {
    this.detailsPrefix = prefix;
    this.detailsSuffix = suffix;
  }

  /**
   * Configure texts to use for the example.
   * 
   * @param prefix Text before each parameter name in the example.
   * @param suffix Text after each parameter name in the example.
   */
  public void setExampleTexts(String prefix, String suffix) {
    this.examplePrefix = prefix;
    this.exampleSuffix = suffix;
  }

  /**
   * Configure the display of tooltip.
   * 
   * @param display True to display tooltip.
   */
  public void setDisplayTooltip(boolean display) {
    this.displayTooltip = display;
  }

  /**
   * Configure texts to use for tooltip.
   * 
   * @param prefixBegin Text before the tooltip value.
   * @param prefixEnd Text between the tooltip value and the text.
   * @param suffix Text after the text.
   */
  public void setTooltipTexts(String prefixBegin, String prefixEnd, String suffix) {
    this.tooltipPrefixBegin = prefixBegin;
    this.tooltipPrefixEnd = prefixEnd;
    this.tooltipSuffix = suffix;
  }

  /**
   * Configure line separation.
   * 
   * @param separation Separation.
   */
  public void setLineSeparation(String separation) {
    this.lineSeparation = separation;
  }

  /**
   * Configure tabulation.
   * 
   * @param tabulation Tabulation.
   */
  public void setLineTabulation(String tabulation) {
    this.lineTabulation = tabulation;
  }

  /**
   * Build the description of an algorithm.
   * 
   * @param algorithm Algorithm.
   * @return Textual description of the algorithm.
   */
  public String describe(Algorithm algorithm) {
    if (algorithm == null) {
      return "";
    }
    StringBuilder result = new StringBuilder();
    if (globalPrefix != null) {
      result.append(globalPrefix);
    }
    if (algorithm.getParameters() != null) {
      List<AlgorithmParameter> tmpParameters = new ArrayList<>(algorithm.getParameters().values());
      tmpParameters.sort(null);
      for (AlgorithmParameter parameter : tmpParameters) {
        addDescription(result, parameter);
      }
    }
    if (globalSuffix != null) {
      result.append(globalSuffix);
    }
    return result.toString();
  }

  /**
   * Build the description of an algorithm parameter.
   * 
   * @param result Buffer to be completed with the description.
   * @param parameter Algorithm parameter to be described.
   */
  private void addDescription(StringBuilder result, AlgorithmParameter parameter) {

    // Add general description of the parameter
    if (parameterPrefix != null) {
      result.append(parameterPrefix);
    }
    if (parameter.getName() != null) {
      if (namePrefix != null) {
        result.append(namePrefix);
      }
      result.append(parameter.getName());
      if (nameSuffix != null) {
        result.append(nameSuffix);
      }
      if (parameter.getDescription() != null) {
        if (descriptionPrefix != null) {
          result.append(descriptionPrefix);
        }
        result.append(parameter.getDescription());
        if (descriptionSuffix != null) {
          result.append(descriptionSuffix);
        }
      }

      // Add detailed description of the parameter
      addDetailedDescription(result, parameter);
    }
    if (parameterSuffix != null) {
      result.append(parameterSuffix);
    }
  }

  /**
   * Build the detailed description of an algorithm parameter.
   * 
   * @param result Buffer to be completed with the description.
   * @param parameter Algorithm parameter to be described.
   */
  private void addDetailedDescription(StringBuilder result, AlgorithmParameter parameter) {
    if (!displayDetails || (parameter == null) || parameter.getElements().isEmpty()) {
      return;
    }

    // Add an example
    if (lineSeparation != null) {
      result.append(lineSeparation);
    }
    if (detailsPrefix != null) {
      result.append(detailsPrefix);
    }
    if (examplePrefix != null) {
      result.append(examplePrefix);
    }
    result.append(parameter.getName());
    if (exampleSuffix != null) {
      result.append(exampleSuffix);
    }
    result.append(" = ");
    if (parameter.isMultiLine()) {
      if (detailsSuffix != null) {
        result.append(detailsSuffix);
      }
      if (lineSeparation != null) {
        result.append(lineSeparation);
      }
      if (lineTabulation != null) {
        result.append(lineTabulation);
      }
      if (detailsPrefix != null) {
        result.append(detailsPrefix);
      }
      addDetailedDescription(result, parameter.getElements());
      if (detailsSuffix != null) {
        result.append(detailsSuffix);
      }
      if (lineSeparation != null) {
        result.append(lineSeparation);
      }
      if (lineTabulation != null) {
        result.append(lineTabulation);
      }
      if (detailsPrefix != null) {
        result.append(detailsPrefix);
      }
      result.append("...");
      if (detailsSuffix != null) {
        result.append(detailsSuffix);
      }
      if (lineSeparation != null) {
        result.append(lineSeparation);
      }
      if (lineTabulation != null) {
        result.append(lineTabulation);
      }
      if (detailsPrefix != null) {
        result.append(detailsPrefix);
      }
      addDetailedDescription(result, parameter.getElements());
    } else {
      addDetailedDescription(result, parameter.getElements());
    }
    result.append(" END");
    if (detailsSuffix != null) {
      result.append(detailsSuffix);
    }

    // Add explanation
    for (AlgorithmParameterElement element : parameter.getElements()) {
      if ((element.getDescription() != null) && !element.getDescription().isEmpty()) {
        if (lineSeparation != null) {
          result.append(lineSeparation);
        }
        if (detailsPrefix != null) {
          result.append(detailsPrefix);
        }
        result.append(element.getText());
        if (detailsSuffix != null) {
          result.append(detailsSuffix);
        }
        result.append(": ");
        result.append(element.getDescription());
      }
    }
  }

  /**
   * Build the detailed description of an algorithm parameter.
   * 
   * @param result Buffer to be completed with the description.
   * @param elements Elements to be described.
   */
  private void addDetailedDescription(StringBuilder result, List<AlgorithmParameterElement> elements) {
    if ((elements == null) || elements.isEmpty()) {
      return;
    }
    int optionalCount = 0;
    boolean first = true;
    for (AlgorithmParameterElement element : elements) {
      if (element.isOptional()) {
        result.append("[");
        optionalCount++;
      }
      if (!first) {
        result.append("|");
      }
      addTooltipText(result, element.getText(), element.getDescription());
      if (element.canBeMultiple()) {
        result.append("...");
      }
      first = false;
    }
    for (int i = 0; i < optionalCount; i++) {
      result.append("]");
    }
  }

  /**
   * Build a text with a tooltip.
   * 
   * @param result Buffer to be completed with the text.
   * @param text Text to display.
   * @param tooltip Text of the tooltip.
   */
  private void addTooltipText(StringBuilder result, String text, String tooltip) {
    if ((text == null) || (text.length() == 0)) {
      return;
    }
    if (!displayTooltip || (tooltip == null) || (tooltip.length() == 0)) {
      result.append(text);
      return;
    }
    if (tooltipPrefixBegin != null) {
      result.append(tooltipPrefixBegin);
    }
    result.append(tooltip);
    if (tooltipPrefixEnd != null) {
      result.append(tooltipPrefixEnd);
    }
    result.append(text);
    if (tooltipSuffix != null) {
      result.append(tooltipSuffix);
    }
  }
}
