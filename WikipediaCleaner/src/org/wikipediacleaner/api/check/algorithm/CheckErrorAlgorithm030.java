/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;

import org.wikipediacleaner.api.check.AddTextActionProvider;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.constants.EnumWikipedia;
import org.wikipediacleaner.api.data.MagicWord;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.gui.swing.action.ActionExternalViewer;
import org.wikipediacleaner.i18n.GT;
import org.wikipediacleaner.utils.StringChecker;
import org.wikipediacleaner.utils.StringCheckerUnauthorizedCharacters;
import org.wikipediacleaner.utils.TextProviderImageDescription;


/**
 * Algorithm for analyzing error 30 of check wikipedia project.
 * Error 30: Image without description
 */
public class CheckErrorAlgorithm030 extends CheckErrorAlgorithmBase {

  /**
   * StringChecker for the description.
   */
  private final StringChecker descriptionChecker;

  public CheckErrorAlgorithm030() {
    super("Image without description");
    descriptionChecker = new StringCheckerUnauthorizedCharacters("[]|=");
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
    if (!analysis.getPage().isArticle()) {
      return false;
    }

    boolean result = false;
    EnumWikipedia wiki = analysis.getWikipedia();
    MagicWord magicWordImgAlt = wiki.getWikiConfiguration().getMagicWordByName(MagicWord.IMG_ALT);
    for (PageElementImage image : analysis.getImages()) {
      String description = image.getDescription();
      if ((description == null) || (description.trim().length() == 0)) {
        String alt = image.getAlternateDescription();
        if ((alt == null) || (alt.trim().length() == 0)) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, image.getBeginIndex(), image.getEndIndex());

          // Action: add a description
          StringBuilder prefixFull = new StringBuilder();
          prefixFull.append("[[");
          prefixFull.append(image.getNamespace());
          prefixFull.append(":");
          prefixFull.append(image.getImage());
          StringBuilder prefixShort = new StringBuilder(prefixFull);
          if (image.getParameters() != null) {
            for (PageElementImage.Parameter param : image.getParameters()) {
              prefixFull.append("|");
              prefixFull.append(param.getContents());
              if (!magicWordImgAlt.isPossibleAlias(param.getContents())) {
                prefixShort.append("|");
                prefixShort.append(param.getContents());
              }
            }
          }
          prefixFull.append("|");
          prefixShort.append("|alt=");
          errorResult.addPossibleAction(
              GT._("Add a description..."),
              new AddTextActionProvider(
                  prefixFull.toString(), "]]",
                  new TextProviderImageDescription(image),
                  GT._("What description would you like to use for the image ?"),
                  descriptionChecker));
          errorResult.addPossibleAction(
              GT._("Add an alternate description..."),
              new AddTextActionProvider(
                  prefixShort.toString(), "]]",
                  new TextProviderImageDescription(image),
                  GT._("What alternate description would you like to use for the image ?"),
                  descriptionChecker));

          // Action: view image
          errorResult.addPossibleAction(new SimpleAction(
              GT._("View image"),
              new ActionExternalViewer(
                  wiki,
                  image.getNamespace() + ":" + image.getImage(),
                  true)));
          errors.add(errorResult);
        }
      }
    }
    return result;
  }
}
