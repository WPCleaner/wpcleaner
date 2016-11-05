/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.HtmlCharacters;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementTemplate;
import org.wikipediacleaner.gui.swing.component.MWPane;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 16 of check wikipedia project.
 * Error 16: Unicode control characters
 */
public class CheckErrorAlgorithm016 extends CheckErrorAlgorithmBase {

  /**
   * Possible global fixes.
   */
  private final static String[] globalFixes = new String[] {
    GT._("Remove all control characters"),
  };

  public CheckErrorAlgorithm016() {
    super("Unicode control characters");
  }

  /**
   * Analyze a page to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param errors Errors found in the page.
   * @param onlyAutomatic True if analysis could be restricted to errors automatically fixed.
   * @return Flag indicating if the error was found.
   */
  @Override
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if (analysis == null) {
      return false;
    }

    // Retrieve configuration
    boolean onlyTemplates = Boolean.valueOf(getSpecificProperty("only_templates", true, true, false));

    boolean result = false;
    String contents = analysis.getContents();

    if (onlyTemplates) {
      Collection<PageElementTemplate> templates = analysis.getTemplates();
      if (templates == null) {
        return false;
      }
      int lastEnd = 0;
      for (PageElementTemplate template : templates) {
        int begin = template.getBeginIndex();
        int end = template.getEndIndex();
        if (begin >= lastEnd) {
          if (analyzeArea(analysis, contents, begin, end, errors)) {
            if (errors == null) {
              return true;
            }
            result = true;
          }
          lastEnd = end;
        }
      }
    } else {
      result = analyzeArea(analysis, contents, 0, contents.length(), errors);
    }

    return result;
  }

  /**
   * Analyze a page area to check if errors are present.
   * 
   * @param analysis Page analysis.
   * @param contents Page contents.
   * @param beginArea Begin index of the area.
   * @param endArea End index of the area.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyzeArea(
      PageAnalysis analysis, String contents, int beginArea, int endArea,
      Collection<CheckErrorResult> errors) {
    boolean result = false;

    // Check every character
    int index = beginArea;
    while (index < endArea) {
      int codePoint = contents.codePointAt(index);
      ControlCharacter control = getControlCharacter(codePoint);
      if (control != null) {
        if (errors == null) {
          return true;
        }
        result = true;

        // Find extent of the area to highlight
        int begin = index;
        if (begin > beginArea) {
          begin = Math.max(index - Character.charCount(contents.codePointBefore(index)), 0);
        }
        List<Integer> controls = new ArrayList<Integer>();
        controls.add(Integer.valueOf(codePoint));
        int end = index + Character.charCount(codePoint);
        while ((end < endArea) &&
               ((getControlCharacter(contents.codePointBefore(end)) != null) ||
                (getControlCharacter(contents.codePointAt(end)) != null))) {
          Integer controlNum = Integer.valueOf(contents.codePointAt(end));
          if ((controlNum != null) && (!controls.contains(controlNum))) {
            controls.add(controlNum);
          }
          end += Character.charCount(contents.codePointAt(end));
        }

        // Report error
        CheckErrorResult errorResult = createCheckErrorResult(analysis, begin, end);
        for (Integer controlFound : controls) {
          ControlCharacter found = getControlCharacter(controlFound.intValue());
          if (found != null) {
            errorResult.addText(
                Integer.toHexString(controlFound.intValue()) + " - " + GT._(found.description));
          }
        }
        StringBuilder replacementB = new StringBuilder();
        List<String> otherReplacements = new ArrayList<String>();
        boolean unsafeCharacter = false;
        boolean checkUnsafe = false;
        int i = begin;
        while (i < end) {
          codePoint = contents.codePointAt(i);
          control = getControlCharacter(codePoint);
          if (control == null) {
            replacementB.appendCodePoint(codePoint);
            unsafeCharacter |= (automaticChars.indexOf(codePoint) < 0);
          } else {
            if (!control.removable) {
              int replaceBy = 0;
              if (control == ControlCharacter.NON_BREAKING_SPACE) {
                if ((i > 0) && (contents.codePointBefore(i) == '«')) {
                  replaceBy = ' ';
                }
                int next = i + Character.charCount(codePoint);
                if (next < end) {
                  int codePointAfter = contents.codePointAt(next);
                  if ((codePointAfter == '»') || (codePointAfter == ':')) {
                    replaceBy = ' ';
                  }
                }
              }

              if (replaceBy == 0) {
                replacementB.appendCodePoint(codePoint);
                checkUnsafe = true;
              } else {
                replacementB.appendCodePoint(replaceBy);
              }
            }
            checkUnsafe |= !control.safe;
            List<String> replacements = ControlCharacter.getReplacements(codePoint);
            if (replacements != null) {
              for (String replacement : replacements) {
                StringBuilder otherReplacement = new StringBuilder();
                int j = begin;
                while (j < end) {
                  int codePointJ = contents.codePointAt(j);
                  if ((i != j) && (codePoint != codePointJ)) {
                    otherReplacement.appendCodePoint(codePointJ);
                  } else {
                    otherReplacement.append(replacement);
                  }
                  j += Character.charCount(codePointJ);
                }
                if (!otherReplacements.contains(otherReplacement.toString())) {
                  otherReplacements.add(otherReplacement.toString());
                }
              }
            }
          }
          i += Character.charCount(codePoint);
        }
        boolean automatic = !unsafeCharacter || !checkUnsafe;
        String original = contents.substring(begin, end);
        String replacement = replacementB.toString();
        if (!replacement.equals(original)) {
          errorResult.addReplacement(
              replacement,
              GT._("Remove all control characters"),
              automatic);
        }
        for (String otherReplacement : otherReplacements) {
          if ((!automatic || replacement.equals(original)) &&
              !otherReplacement.equals(original) &&
              !otherReplacement.equals(replacement)) {
            errorResult.addReplacement(otherReplacement);
          }
        }
        errors.add(errorResult);
        index = end;
      } else {
        index += Character.charCount(codePoint);
      }
    }

    return result;
  }

  /**
   * Authorized characters for automatic replacement.
   */
  private final static String automaticChars =
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ" +
      "abcdefghijklmnopqrstuvwxyz" +
      "áàâäåãÀ" + "éèêëÉ" + "íìîïĩ" + "óôöōŌ" + "úùûü" + "ý" +
      "ćč" + "ńň" + "š" + "ź" +
      "0123456789" +
      " []|(){}<>,.!?;:--–=+*#/%'\"«»\n\t";

  /**
   * @param codePoint Code point.
   * @return Control character related to the code point.
   */
  private ControlCharacter getControlCharacter(int codePoint) {
    ControlCharacter control = ControlCharacter.getControlCharacter(codePoint);
    if (control != null) {
      String propertyName = "use_" + Integer.toHexString(codePoint).toUpperCase();
      String filter = getSpecificProperty(propertyName, true, true, false);
      if ((filter != null) && (Boolean.FALSE.equals(Boolean.valueOf(filter)))) {
        control = null;
      }
    }
    return control;
  }

  /**
   * Automatic fixing of all the errors in the page.
   * 
   * @param analysis Page analysis.
   * @return Page contents after fix.
   */
  @Override
  protected String internalAutomaticFix(PageAnalysis analysis) {
    return fix(globalFixes[0], analysis, null);
  }

  /**
   * @return List of possible global fixes.
   */
  @Override
  public String[] getGlobalFixes() {
    return globalFixes;
  }

  /**
   * Fix all the errors in the page.
   * 
   * @param fixName Fix name (extracted from getGlobalFixes()).
   * @param analysis Page analysis.
   * @param textPane Text pane.
   * @return Page contents after fix.
   */
  @Override
  public String fix(String fixName, PageAnalysis analysis, MWPane textPane) {
    return fixUsingAutomaticReplacement(analysis);
  }

  /**
   * @return Map of parameters (Name -> description).
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#getParameters()
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put("only_templates", GT._("To report control characters only in templates"));
    return parameters;
  }

  /**
   * Control characters characteristics.
   */
  private enum ControlCharacter {
    DELETE(0x007F, 0x007F, true, false, GT._No("Delete")),
    NON_BREAKING_SPACE(0x00A0, 0x00A0, false, true, GT._No("Non-breaking space")),
    SOFT_HYPHEN(0x00AD, 0x00AD, true, true, GT._No("Soft hyphen")),
    THREE_PER_EM_SPACE(0x2004, 0x2004, true, false, GT._No("Thee-per-em space")),
    FOUR_PER_EM_SPACE(0x2005, 0x2005, true, false, GT._No("Four-per-em space")),
    SIX_PER_EM_SPACE(0x2006, 0x2006, true, false, GT._No("Six-per-em space")),
    FIGURE_SPACE(0x2007, 0x2007, true, false, GT._No("Figure space")),
    PUNCTUATION_SPACE(0x2008, 0x2008, true, false, GT._No("Punctuation space")),
    ZERO_WIDTH_SPACE(0x200B, 0x200B, true, false, GT._No("Zero-width space")),
    LEFT_TO_RIGHT_MARK(0x200E, 0x200E, true, false, GT._No("Left-to-right mark")),
    LINE_SEPARATOR(0x2028, 0x2028, true, false, GT._No("Line separator")),
    LEFT_TO_RIGHT_EMBEDDING(0x202A, 0x202A, true, false, GT._No("Left-to-right embedding")),
    POP_DIRECTIONAL_FORMATTING(0x202C, 0x202C, true, false, GT._No("Pop directional formatting")),
    BYTE_ORDER_MARK(0xFEFF, 0xFEFF, true, false, GT._No("Byte order mark")),
    OBJECT_REPLACEMENT_CHARACTER(0xFFFC, 0xFFFC, true, false, GT._No("Object replacement character")),
    PUA(0xE000, 0xF8FF, false, false, GT._No("Private use area")),
    PUA_A(0XF0000, 0xFFFFD, false, false, GT._No("Private use area A")),
    PUA_B(0x100000, 0x10FFFD, false, false, GT._No("Private use area B"));

    public final int begin;
    public final int end;
    public final boolean removable;
    public final boolean safe;
    public final String description;

    /**
     * @param begin Begin of the range of control characters.
     * @param end End of the range of control characters.
     * @param removable True if the control character can be removed.
     * @param safe True if removing the control character is safe.
     * @param description Description of the control character.
     */
    private ControlCharacter(
        int begin, int end,
        boolean removable, boolean safe,
        String description) {
      this.begin = begin;
      this.end = end;
      this.removable = removable;
      this.safe = safe;
      this.description = description;
    }

    /**
     * @param codePoint Code point.
     * @return Control character for the given code point.
     */
    public static ControlCharacter getControlCharacter(int codePoint) {
      for (ControlCharacter control : values()) {
        if ((codePoint >= control.begin) && (codePoint <= control.end)) {
          return control;
        }
      }
      return null;
    }

    /**
     * @param codePoint Character.
     * @param control Control characters set.
     * @return True if the character is in the control character set.
     */
    public static boolean isIncluded(int codePoint, ControlCharacter control) {
      if (control == null) {
        return false;
      }
      if ((codePoint >= control.begin) && (codePoint <= control.end)) {
        return true;
      }
      return false;
    }

    /**
     * @param codePoint Code point.
     * @return Potential replacement.
     */
    public static List<String> getReplacements(int codePoint) {
      List<String> replacements = null;
      // TODO: Test replacing left to right mark with HTML character
      /*if (codePoint == HtmlCharacters.LEFT_TO_RIGHT_MARK.getValue()) {
        replacements = Collections.singletonList(HtmlCharacters.LEFT_TO_RIGHT_MARK.getFullEntity());
      }*/
      if (codePoint == HtmlCharacters.SYMBOL_NON_BREAKING_SPACE.getValue()) {
        replacements = new ArrayList<>();
        replacements.add(" ");
        replacements.add(HtmlCharacters.SYMBOL_NON_BREAKING_SPACE.getFullEntity());
      }
      if (codePoint == HtmlCharacters.SYMBOL_SOFT_HYPHEN.getValue()) {
        replacements = new ArrayList<>();
        replacements.add(HtmlCharacters.SYMBOL_SOFT_HYPHEN.getFullEntity());
        replacements.add("-");
        replacements.add("");
      }
      if (isIncluded(codePoint, THREE_PER_EM_SPACE) ||
          isIncluded(codePoint, FOUR_PER_EM_SPACE) ||
          isIncluded(codePoint, SIX_PER_EM_SPACE) ||
          isIncluded(codePoint, FIGURE_SPACE) ||
          isIncluded(codePoint, PUNCTUATION_SPACE)) {
        replacements = new ArrayList<>();
        replacements.add(" ");
        replacements.add("");
      }
      if (codePoint == 0xF0FC) {
        replacements = new ArrayList<>();
        replacements.add("*");
        replacements.add("✓");
      }
      return replacements;
    }
  }
}
