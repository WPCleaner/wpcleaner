/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
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

package org.wikipediacleaner.api.check.algorithm;

import java.util.Collection;

import org.wikipediacleaner.api.check.AddTextActionProvider;
import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.SimpleAction;
import org.wikipediacleaner.api.data.MagicWord;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.gui.swing.action.PageViewAction;
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
   * @param pageAnalysis Page analysis.
   * @param errors Errors found in the page.
   * @return Flag indicating if the error was found.
   */
  public boolean analyze(
      PageAnalysis pageAnalysis,
      Collection<CheckErrorResult> errors) {
    if (pageAnalysis == null) {
      return false;
    }

    boolean result = false;
    MagicWord magicWordImgAlt = pageAnalysis.getWikipedia().getWikiConfiguration().getMagicWord(MagicWord.IMG_ALT);
    for (PageElementImage image : pageAnalysis.getImages()) {
      String description = image.getDescription();
      if ((description == null) || (description.trim().length() == 0)) {
        String alt = image.getAlternateDescription();
        if ((alt == null) || (alt.trim().length() == 0)) {
          if (errors == null) {
            return true;
          }
          result = true;
          CheckErrorResult errorResult = createCheckErrorResult(
              pageAnalysis.getPage(), image.getBeginIndex(), image.getEndIndex());

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
                  GT._("What description would like to use for the image ?"),
                  descriptionChecker));
          errorResult.addPossibleAction(
              GT._("Add an alternate description..."),
              new AddTextActionProvider(
                  prefixShort.toString(), "]]",
                  new TextProviderImageDescription(image),
                  GT._("What alternate description would like to use for the image ?"),
                  descriptionChecker));

          // Action: view image
          errorResult.addPossibleAction(new SimpleAction(
              GT._("View image"),
              new PageViewAction(
                  image.getNamespace() + ":" + image.getImage(),
                  pageAnalysis.getWikipedia(),
                  true)));
          errors.add(errorResult);
        }
      }
    }
    return result;
  }
}
