/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2008  Nicolas Vervelle
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package org.wikipediacleaner.api.check;

import java.util.ArrayList;

import org.wikipediacleaner.i18n.GT;


/**
 * A class for memorizing informations about errors detected.
 */
public class CheckErrorResult {

  private final String errorType;
  private final int startPosition;
  private final int endPosition;
  private final ErrorLevel errorLevel;

  private ArrayList<Actionnable> possibleActions;
  private ArrayList<Actionnable> possibleReplacements;

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
   * @param errorType Type of error.
   * @param startPosition Start of the error.
   * @param endPosition End of the error.
   * @param errorLevel Error level.
   */
  public CheckErrorResult(
      String errorType,
      int startPosition, int endPosition,
      ErrorLevel errorLevel) {
    this.errorType = errorType;
    this.startPosition = startPosition;
    this.endPosition = endPosition;
    this.errorLevel = errorLevel;
    this.possibleActions = new ArrayList<Actionnable>();
    this.possibleReplacements = null;
  }

  /**
   * Constructor.
   * 
   * @param errorType Type of error.
   * @param startPosition Start of the error.
   * @param endPosition End of the error.
   */
  public CheckErrorResult(
      String errorType,
      int startPosition, int endPosition) {
    this(errorType, startPosition, endPosition, ErrorLevel.ERROR);
  }

  /**
   * @return Type of error.
   */
  public String getErrorType() {
    return errorType;
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
    addReplacement(
        replacement,
        (replacement.length() > 0) ?
            GT._("Replace with {0}", replacement) :
            GT._("Delete"));
  }

  /**
   * Add a possible replacement for the error.
   * 
   * @param replacement Possible replacement.
   * @param text Text explaining the replacement.
   */
  public void addReplacement(String replacement, String text) {
    if (replacement == null) {
      return;
    }
    //replacement = replacement.trim();
    if (possibleReplacements == null) {
      possibleReplacements = new ArrayList<Actionnable>();
    }
    for (Actionnable actionnable : possibleReplacements) {
      if (replacement.equals(actionnable.getName())) {
        return;
      }
    }
    SimpleAction action = new SimpleAction(
        text,
        new ReplaceTextActionProvider(replacement));
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
  public ArrayList<Actionnable> getPossibleActions() {
    return possibleActions;
  }
}
