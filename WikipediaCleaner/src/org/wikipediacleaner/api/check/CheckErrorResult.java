/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check;

import java.util.ArrayList;
import java.util.List;

import org.wikipediacleaner.api.algorithm.Algorithm;
import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.api.data.Page;
import org.wikipediacleaner.api.data.PageElementTitle;
import org.wikipediacleaner.api.data.Replacement;
import org.wikipediacleaner.gui.swing.component.MWPaneEditTocAction;
import org.wikipediacleaner.i18n.GT;


/**
 * A class for memorizing information about errors detected.
 */
public class CheckErrorResult implements Comparable<CheckErrorResult> {

  private final Algorithm algorithm;
  private final Page page;
  private final int startPosition;
  private final int endPosition;
  private final ErrorLevel errorLevel;

  private List<Actionnable> possibleActions;
  private List<Actionnable> possibleReplacements;

  /**
   * Error levels.
   */
  public static enum ErrorLevel {
    ERROR("Error"),
    WARNING("Warning"),
    CORRECT("Correct");

    /** Description of error level */
    private final String description;

    /**
     * @param description Description of the error level.
     */
    private ErrorLevel(String description) {
      this.description = description;
    }

    /**
     * @return Description of the error level.
     * @see java.lang.Enum#toString()
     */
    @Override
    public String toString() {
      return description;
    }

  }

  /**
   * Constructor.
   * 
   * @param algorithm Type of error.
   * @param page Page.
   * @param startPosition Start of the error.
   * @param endPosition End of the error.
   * @param errorLevel Error level.
   */
  public CheckErrorResult(
      CheckErrorAlgorithm algorithm,
      Page page,
      int startPosition, int endPosition,
      ErrorLevel errorLevel) {
    this.algorithm = algorithm;
    this.page = page;
    this.startPosition = startPosition;
    this.endPosition = endPosition;
    this.errorLevel = errorLevel;
    this.possibleActions = new ArrayList<>();
    this.possibleReplacements = null;
  }

  /**
   * Constructor.
   * 
   * @param algorithm Type of error.
   * @param page Page.
   * @param startPosition Start of the error.
   * @param endPosition End of the error.
   */
  public CheckErrorResult(
      CheckErrorAlgorithm algorithm,
      Page page,
      int startPosition, int endPosition) {
    this(algorithm, page, startPosition, endPosition, ErrorLevel.ERROR);
  }

  /**
   * @return Type of error.
   */
  public Algorithm getAlgorithm() {
    return this.algorithm;
  }

  /**
   * @return Type of error.
   */
  public String getErrorType() {
    if (algorithm == null) {
      return null;
    }
    return algorithm.getShortDescription();
  }

  /**
   * @return Start of the error.
   */
  public int getStartPosition() {
    return startPosition;
  }

  /**
   * @return End of the error.
   */
  public int getEndPosition() {
    return endPosition;
  }

  /**
   * @return Length of the error.
   */
  public int getLength() {
    return endPosition - startPosition;
  }

  /**
   * @return Error level.
   */
  public ErrorLevel getErrorLevel() {
    return errorLevel;
  }

  /**
   * Add a possible replacement for the error.
   * 
   * @param replacement Possible replacement.
   */
  public void addReplacement(Replacement replacement) {
    if (replacement == null) {
      return;
    }
    if (replacement.text == null) {
      addReplacement(
          replacement.replacement,
          replacement.automatic, replacement.automaticBot);
    } else {
      addReplacement(
          replacement.replacement, replacement.text,
          replacement.automatic, replacement.automaticBot);
    }
  }

  /**
   * Add a possible replacement for the error.
   * 
   * @param replacement Possible replacement.
   */
  public void addReplacement(String replacement) {
    addReplacement(replacement, false);
  }

  /**
   * Add a possible replacement for the error.
   * 
   * @param replacement Possible replacement.
   * @param automatic True if replacement can be done automatically.
   */
  public void addReplacement(String replacement, boolean automatic) {
    addReplacement(replacement, automatic, automatic);
  }

  /**
   * Add a possible replacement for the error.
   * 
   * @param replacement Possible replacement.
   * @param automatic True if replacement can be done automatically.
   * @param automaticBot True if replacement can be done automatically in bot mode.
   */
  public void addReplacement(
      String replacement,
      boolean automatic, boolean automaticBot) {
    addReplacement(
        replacement,
        (replacement.length() > 0) ?
            GT._T("Replace with {0}", replacement.replaceAll("\\n", "\u21b5")) :
            GT._T("Delete"),
        automatic, automaticBot);
  }

  /**
   * Add a possible replacement for the error.
   * 
   * @param replacement Possible replacement.
   * @param text Text explaining the replacement.
   */
  public void addReplacement(String replacement, String text) {
    addReplacement(replacement, text, false);
  }

  /**
   * Add a possible replacement for the error.
   * 
   * @param replacement Possible replacement.
   * @param text Text explaining the replacement.
   * @param automatic True if replacement can be done automatically.
   */
  public void addReplacement(
      String replacement, String text,
      boolean automatic) {
    addReplacement(replacement, text, automatic, automatic);
  }

  /**
   * Add a possible replacement for the error.
   * 
   * @param replacement Possible replacement.
   * @param text Text explaining the replacement.
   * @param automatic True if replacement can be done automatically.
   * @param automaticBot True if replacement can be done automatically in bot mode.
   */
  public void addReplacement(
      String replacement, String text,
      boolean automatic, boolean automaticBot) {
    if (replacement == null) {
      return;
    }
    //replacement = replacement.trim();
    if (possibleReplacements == null) {
      possibleReplacements = new ArrayList<>();
    }
    for (Actionnable actionnable : possibleReplacements) {
      if (text.equals(actionnable.getName())) {
        return;
      }
    }
    SimpleAction action = new SimpleAction(
        text,
        new ReplaceTextActionProvider(
            page, replacement, automatic, automaticBot));
    possibleActions.add(action);
    possibleReplacements.add(action);
  }

  /**
   * @return First replacement.
   */
  public String getFirstReplacement() {
    if (possibleReplacements == null) {
      return null;
    }
    if (possibleReplacements.isEmpty()) {
      return null;
    }
    Actionnable action = possibleReplacements.get(0);
    if (!(action instanceof SimpleAction)) {
      return null;
    }
    ActionProvider provider = ((SimpleAction) action).getActionProvider();
    if (!(provider instanceof ReplaceTextActionProvider)) {
      return null;
    }
    return ((ReplaceTextActionProvider) provider).getFinalizedNewText();
  }

  /**
   * @return Automatic replacement.
   */
  public String getAutomaticReplacement() {
    if (possibleReplacements == null) {
      return null;
    }
    for (Actionnable action : possibleReplacements) {
      if (action instanceof SimpleAction) {
        ActionProvider provider = ((SimpleAction) action).getActionProvider();
        if (provider instanceof ReplaceTextActionProvider) {
          ReplaceTextActionProvider textProvider = (ReplaceTextActionProvider) provider;
          if (textProvider.isAutomatic()) {
            return textProvider.getFinalizedNewText();
          }
        }
      }
    }
    return null;
  }

  /**
   * @return Automatic bot replacement.
   */
  public String getAutomaticBotReplacement() {
    if (possibleReplacements == null) {
      return null;
    }
    for (Actionnable action : possibleReplacements) {
      if (action instanceof SimpleAction) {
        ActionProvider provider = ((SimpleAction) action).getActionProvider();
        if (provider instanceof ReplaceTextActionProvider) {
          ReplaceTextActionProvider textProvider = (ReplaceTextActionProvider) provider;
          if (textProvider.isAutomaticBot()) {
            return textProvider.getFinalizedNewText();
          }
        }
      }
    }
    return null;
  }

  /**
   * Add an action for editing the table of contents.
   * 
   * @param title Title to select by default when opening the table of contents.
   */
  public void addEditTocAction(PageElementTitle title) {
    addPossibleAction(
        new SimpleAction(GT._T("Edit table of contents"),
        new MWPaneEditTocAction(title)));
  }

  /**
   * Add a possible action.
   * 
   * @param action Action.
   */
  public void addPossibleAction(Actionnable action) {
    if (action != null) {
      possibleActions.add(action);
    }
  }

  /**
   * Add a possible action.
   * 
   * @param name Action name.
   * @param action Action provider.
   */
  public void addPossibleAction(String name, ActionProvider action) {
    if ((name != null) && (action != null)) {
      addPossibleAction(new SimpleAction(name, action));
    }
  }

  /**
   * Add grayed text.
   * 
   * @param name Action name.
   */
  public void addText(String name) {
    addPossibleAction(name, new NullActionProvider());
  }

  /**
   * @return Possible actions.
   */
  public List<Actionnable> getPossibleActions() {
    return possibleActions;
  }

  /**
   * @param cer Other check error result.
   * @return Comparison of the two check error results.
   * @see java.lang.Comparable#compareTo(java.lang.Object)
   */
  @Override
  public int compareTo(CheckErrorResult cer) {
    if (cer == null) {
      return -1;
    }
    if (startPosition != cer.startPosition) {
      return startPosition - cer.startPosition;
    }
    if (endPosition != cer.endPosition) {
      return -(endPosition - cer.endPosition);
    }
    return 0;
  }
}
