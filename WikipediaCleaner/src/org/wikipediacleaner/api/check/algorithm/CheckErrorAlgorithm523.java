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
import org.wikipediacleaner.api.data.Namespace;
import org.wikipediacleaner.api.data.PageAnalysis;
import org.wikipediacleaner.api.data.PageElementImage;
import org.wikipediacleaner.api.data.PageElementTag;


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
  public boolean analyze(
      PageAnalysis analysis,
      Collection<CheckErrorResult> errors, boolean onlyAutomatic) {
    if ((analysis == null) || (analysis.getPage() == null)) {
      return false;
    }

    // Memorize where each single image is
    Map<String, List<Element>> imagesMap = new HashMap<String, List<Element>>();
    List<PageElementImage> images = analysis.getImages();
    for (PageElementImage image : images) {
      addImage(imagesMap, image.getImage(), image.getBeginIndex(), image.getEndIndex());
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
    if (imagesMap == null) {
      return;
    }
    List<Element> elements = imagesMap.get(imageName);
    if (elements == null) {
      elements = new ArrayList<Element>();
      imagesMap.put(imageName, elements);
    }
    elements.add(new Element(beginIndex, endIndex));
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
