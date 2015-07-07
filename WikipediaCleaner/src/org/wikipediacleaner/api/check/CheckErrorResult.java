/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check;

import java.util.ArrayList;
import java.util.List;

import org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithm;
import org.wikipediacleaner.gui.swing.component.MWPaneEditTocAction;
import org.wikipediacleaner.i18n.GT;


/**
 * A class for memorizing information about errors detected.
 */
public class CheckErrorResult implements Comparable<CheckErrorResult> {

  private final CheckErrorAlgorithm algorithm;
  private final int startPosition;
  private final int endPosition;
  private final ErrorLevel errorLevel;

  private List<Actionnable> possibleActions;
  private List<Actionnable> possibleReplacements;

  /**
   * Error levels.
   */
  public static enum ErrorLevel {
    ERROR(),
    WARNING(),
    CORRECT();
  }

  /**
   * Constructor.
   * 
   * @param algorithm Type of error.
   * @param startPosition Start of the error.
   * @param endPosition End of the error.
   * @param errorLevel Error level.
   */
  public CheckErrorResult(
      CheckErrorAlgorithm algorithm,
      int startPosition, int endPosition,
      ErrorLevel errorLevel) {
    this.algorithm = algorithm;
    this.startPosition = startPosition;
    this.endPosition = endPosition;
    this.errorLevel = errorLevel;
    this.possibleActions = new ArrayList<Actionnable>();
    this.possibleReplacements = null;
  }

  /**
   * Constructor.
   * 
   * @param algorithm Type of error.
   * @param startPosition Start of the error.
   * @param endPosition End of the error.
   */
  public CheckErrorResult(
      CheckErrorAlgorithm algorithm,
      int startPosition, int endPosition) {
    this(algorithm, startPosition, endPosition, ErrorLevel.ERROR);
  }

  /**
   * @return Type of error.
   */
  public CheckErrorAlgorithm getAlgorithm() {
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
            GT._("Replace with {0}", replacement.replaceAll("\\n", "\u21b5")) :
            GT._("Delete"),
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
      possibleReplacements = new ArrayList<Actionnable>();
    }
    for (Actionnable actionnable : possibleReplacements) {
      if (text.equals(actionnable.getName())) {
        return;
      }
    }
    SimpleAction action = new SimpleAction(
        text,
        new ReplaceTextActionProvider(
            replacement, automatic, automaticBot));
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
    return ((ReplaceTextActionProvider) provider).getNewText();
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
            return textProvider.getNewText();
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
            return textProvider.getNewText();
          }
        }
      }
    }
    return null;
  }

  /**
   * Add an action for editing the table of contents.
   */
  public void addEditTocAction() {
    addPossibleAction(
        new SimpleAction(GT._("Edit table of contents"),
        new MWPaneEditTocAction()));
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
      return endPosition - cer.endPosition;
    }
    return 0;
  }
}
