/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.check.algorithm;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.wikipediacleaner.api.check.CheckErrorResult;
import org.wikipediacleaner.api.check.CheckErrorResult.ErrorLevel;
import org.wikipediacleaner.api.data.MagicWord;
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.api.data.PageElementTag;
import org.wikipediacleaner.i18n.GT;


/**
 * Algorithm for analyzing error 523 of check wikipedia project.
 * Error 523: Duplicated image
 */
public class CheckErrorAlgorithm523 extends CheckErrorAlgorithmBase {

  public CheckErrorAlgorithm523() {
    super("Duplicated image");
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
    if ((analysis == null) || (analysis.getPage() == null)) {
      return false;
    }

    // Configuration
    int minSize = 64;
    String minSizeText = getSpecificProperty("min_size", true, true, false);
    if (minSizeText != null) {
      try {
        minSize = Integer.parseInt(minSizeText, 10);
      } catch (NumberFormatException e) {
        // Nothing to do
      }
    }

    // Memorize where each single image is
    Map<String, List<Element>> imagesMap = new HashMap<String, List<Element>>();
    List<PageElementImage> images = analysis.getImages();
    for (PageElementImage image : images) {
      boolean shouldAdd = true;

      // Only images that respect a minimum size
      if (shouldAdd) {
        PageElementImage.Parameter paramWidth = image.getParameter(MagicWord.IMG_WIDTH);
        if ((paramWidth != null) && (paramWidth.getContents() != null)) {
          String contents = paramWidth.getContents().replaceAll("\\D", "");
          try {
            int size = Integer.parseInt(contents, 10);
            if (size < minSize) {
              shouldAdd = false;
            }
          } catch (NumberFormatException e) {
            // Nothing to do
          }
        }
      }

      // Ignore images with a page parameter
      if (shouldAdd) {
        PageElementImage.Parameter paramPage = image.getParameter(MagicWord.IMG_PAGE);
        if (paramPage != null) {
          shouldAdd = false;
        }
      }

      if (shouldAdd) {
        addImage(imagesMap, image.getImage(), image.getBeginIndex(), image.getEndIndex());
      }
    }

    // Memorize where each image in a gallery is
    List<PageElementTag> galleryTags = analysis.getCompleteTags(PageElementTag.TAG_WIKI_GALLERY);
    String contents = analysis.getContents();
    Namespace imageNamespace = analysis.getWikiConfiguration().getNamespace(Namespace.IMAGE);
    for (PageElementTag galleryTag : galleryTags) {
      if (galleryTag.getMatchingTag() != null) {
        PageElementTag endTag = galleryTag.getMatchingTag();
        int beginIndex = galleryTag.getEndIndex();
        int tmpIndex = beginIndex;
        while (tmpIndex <= endTag.getBeginIndex()) {
          if ((tmpIndex == endTag.getBeginIndex()) ||
              (contents.charAt(tmpIndex) == '\n')) {
            String line = contents.substring(beginIndex, tmpIndex).trim();
            int colonIndex = line.indexOf(':');
            if ((colonIndex > 0) && (imageNamespace.isPossibleName(line.substring(0, colonIndex)))) {
              String imageName = line.substring(colonIndex + 1);
              int pipeIndex = imageName.indexOf('|', colonIndex);
              if (pipeIndex > 0) {
                imageName = imageName.substring(0, pipeIndex);
              }
              int beginImageIndex = beginIndex;
              int endImageIndex = beginImageIndex + colonIndex + 1 + imageName.length();
              addImage(imagesMap, imageName, beginImageIndex, endImageIndex);
            }
            beginIndex = tmpIndex + 1;
          }
          tmpIndex++;
        }
      }
    }

    // Analyze each title
    boolean result = false;
    for (List<Element> elements : imagesMap.values()) {
      if (elements.size() > 1) {
        if (errors == null) {
          return true;
        }
        result = true;
        for (int elementNum = 0; elementNum < elements.size(); elementNum++) {
          Element element = elements.get(elementNum);
          CheckErrorResult errorResult = createCheckErrorResult(
              analysis, element.beginIndex, element.endIndex,
              (elementNum == 0) ? ErrorLevel.WARNING : ErrorLevel.ERROR);
          errors.add(errorResult);
        }
      }
    }

    return result;
  }

  /**
   * Add an image in the map.
   * 
   * @param imagesMap Map of images.
   * @param imageName Name of the image.
   * @param beginIndex Begin index of the image position.
   * @param endIndex End index of the image position.
   */
  private void addImage(
      Map<String, List<Element>> imagesMap,
      String imageName,
      int beginIndex, int endIndex) {
    if ((imagesMap == null) || (imageName == null)) {
      return;
    }
    boolean shouldAdd = true;
    if (shouldAdd) {
      if (imageName.endsWith(".svg")) {
        shouldAdd = false;
      }
    }
    if (shouldAdd) {
      List<Element> elements = imagesMap.get(imageName);
      if (elements == null) {
        elements = new ArrayList<Element>();
        imagesMap.put(imageName, elements);
      }
      elements.add(new Element(beginIndex, endIndex));
    }
  }

  /**
   * @return Map of parameters (Name -> description).
   * @see org.wikipediacleaner.api.check.algorithm.CheckErrorAlgorithmBase#getParameters()
   */
  @Override
  public Map<String, String> getParameters() {
    Map<String, String> parameters = super.getParameters();
    parameters.put("min_size", GT._("The size below which images are not reported as duplicates"));
    return parameters;
  }

  /**
   * Bean for holding information about an image element.
   */
  private static class Element {
    public final int beginIndex;
    public final int endIndex;

    public Element(int beginIndex, int endIndex) {
      this.beginIndex = beginIndex;
      this.endIndex = endIndex;
    }
  }
}
