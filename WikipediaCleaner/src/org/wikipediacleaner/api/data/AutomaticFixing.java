/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.util.Collection;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;


/**
 * Utility class to memorize automatic fixing parameters
 */
@XmlRootElement(name="replacement")
@XmlType
public class AutomaticFixing implements Comparable<AutomaticFixing> {

  /**
   * Original text.
   */
  private String originalText;

  /**
   * Replacement text.
   */
  private String replacementText;

  /**
   * Regular expression or not.
   */
  private boolean regex;

  /**
   * Default constructor.
   */
  public AutomaticFixing() {
    //
  }

  /**
   * Constructor.
   * 
   * @param from Original text.
   * @param to Replacement text.
   * @param regex Regular expression or not.
   */
  public AutomaticFixing(String from, String to, boolean regex) {
    originalText = from;
    replacementText = to;
    this.regex = regex;
  }

  /**
   * @return Original text.
   */
  public String getOriginalText() {
    return originalText;
  }

  /**
   * @param text Original text.
   */
  public void setOriginalText(String text) {
    originalText = text;
  }

  /**
   * @return Replacement text.
   */
  public String getReplacementText() {
    return replacementText;
  }

  /**
   * @param text Replacement text.
   */
  public void setReplacementText(String text) {
    replacementText = text;
  }

  /**
   * @return Regular expression or not.
   */
  public Boolean getRegex() {
    return Boolean.valueOf(regex);
  }

  /**
   * @param flag Regular expression or not.
   */
  public void setRegex(Boolean flag) {
    regex = Boolean.TRUE.equals(flag);
  }

  /**
   * Apply a list of automatic fixing expressions to a text.
   * 
   * @param fixing List of automatic fixing expressions.
   * @param text Original text.
   * @param replacements Optional list of replacements performed.
   * @return Text with replacements done.
   */
  public static String apply(
      Collection<AutomaticFixing> fixing, String text,
      List<String> replacements) {
    if ((text == null) || (fixing == null) || (fixing.isEmpty())) {
      return text;
    }

    // Apply each automatic fixing expression to the text
    StringBuffer tmpText = new StringBuffer();
    for (AutomaticFixing replacement : fixing) {

      // Initialize data
      tmpText.setLength(0);
      String originalText = replacement.getOriginalText();
      String replacementText = replacement.getReplacementText();
      if (replacementText == null) {
        replacementText = "";
      }

      int currentIndex = 0;
      if (replacement.regex) {
        // Apply for a regular expression
        try {
          Pattern pattern = Pattern.compile(originalText);
          Matcher matcher = pattern.matcher(text);
          while (matcher.find()) {
            String foundText = matcher.group();
            int start = matcher.start();
            int end = matcher.end();
            int currentLength = tmpText.length();
            matcher.appendReplacement(tmpText, replacementText);
            if (replacements != null) {
              String replacedBy = tmpText.substring(currentLength + start - currentIndex);
              String comment = foundText + " → " + replacedBy;
              if (!replacements.contains(comment)) {
                replacements.add(comment);
              }
            }
            currentIndex = end;
          }
        } catch (PatternSyntaxException e) {
          System.err.println("Error with " + originalText + ": " + e.getMessage());
        }
      } else {
        // Apply for a basic expression
        boolean finished = false;
        while (!finished) {
          int newIndex = text.indexOf(originalText, currentIndex);
          if (newIndex < 0) {
            finished = true;
          } else {
            if (newIndex > currentIndex) {
              tmpText.append(text.substring(currentIndex, newIndex));
              currentIndex = newIndex;
            }
            tmpText.append(replacementText);
            currentIndex += originalText.length();
            if (replacements != null) {
              String comment = originalText + " → " + replacementText;
              if (!replacements.contains(comment)) {
                replacements.add(comment);
              }
            }
          }
        }
      }
      if (currentIndex > 0) {
        if (currentIndex < text.length()) {
          tmpText.append(text.substring(currentIndex));
        }
        text = tmpText.toString();
      }
    }

    return text;
  }

  /**
   * @param af Other automatic fixing expression.
   * @return Comparison of the two automatic fixing expressions.
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  @Override
  public int compareTo(AutomaticFixing af) {
    if (af == null) {
      return -1;
    }
    int compare = originalText.compareTo(af.getOriginalText());
    if (compare != 0) {
      return compare;
    }
    return replacementText.compareTo(af.getReplacementText());
  }

  /**
   * @return String representation of the automatic fixing expression.
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    return originalText + " → " + replacementText;
  }
}
