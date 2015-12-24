/*
 *  WPCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2013  Nicolas Vervelle
 *
 *  See README.txt file for licensing information.
 */

package org.wikipediacleaner.api.data;

import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;


/**
 * Management of non wiki text areas.
 */
public class PageElementAreas {

  /**
   * List of non wiki text areas.
   */
  private final List<Area> areas;

  /**
   * Initialize areas.
   */
  public PageElementAreas() {
    areas = new LinkedList<PageElementAreas.Area>();
  }

  /**
   * @return List of areas.
   */
  public List<Area> getAreas() {
    return Collections.unmodifiableList(areas);
  }

  /**
   * @param index Current index.
   * @return First index after area.
   */
  public int getEndArea(int index) {
    for (Area area : areas) {
      if (area.beginIndex > index) {
        return index;
      }
      if (area.endIndex > index) {
        return area.endIndex;
      }
    }
    return index;
  }

  /**
   * Add comments to non wiki text areas.
   * 
   * @param comments List of comments.
   */
  public void addComments(List<PageElementComment> comments) {
    addPageElements(comments);
  }

  /**
   * Add tags to non wiki text areas.
   * @param tags List of tags.
   */
  public void addTags(List<PageElementTag> tags) {
    if (tags != null) {
      for (PageElementTag tag : tags) {
        if (!tag.isFullTag() &&
            (PageElementTag.TAG_WIKI_NOWIKI.equals(tag.getNormalizedName()) ||
             PageElementTag.TAG_WIKI_MATH.equals(tag.getNormalizedName()))) {
          if (!tag.isEndTag() || !tag.isComplete()) {
            addArea(tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());
          }
        } else {
          addArea(tag.getBeginIndex(), tag.getEndIndex());
        }
      }
    }
  }

  /**
   * Add internal links to non wiki text areas.
   * 
   * @param links List of internal links.
   */
  public void addInternalLinks(List<PageElementInternalLink> links) {
    if (links != null) {
      for (PageElementInternalLink link : links) {
        int beginIndex = link.getBeginIndex();
        int endIndex = link.getEndIndex();
        if (link.getText() != null) {
          addArea(beginIndex, beginIndex + link.getTextOffset());
          addArea(endIndex - 2, endIndex);
        } else {
          addArea(beginIndex, endIndex);
        }
      }
    }
  }

  /**
   * Add images to non wiki text areas.
   * 
   * @param images List of images.
   */
  public void addImages(List<PageElementImage> images) {
    if (images != null) {
      for (PageElementImage image : images) {
        int beginIndex = image.getBeginIndex();
        int endIndex = image.getEndIndex();
        if ((image.getFirstPipeOffset() < 0) ||
            (image.getParameters() == null)) {
          addArea(beginIndex, endIndex);
        } else {
          addArea(beginIndex, beginIndex + image.getFirstPipeOffset() + 1);
          for (PageElementImage.Parameter param : image.getParameters()) {
            MagicWord magicWord = param.getMagicWord();
            if (magicWord != null) {
              if (MagicWord.IMG_ALT.equals(magicWord.getName())) {
                int equalIndex = param.getContents().indexOf("=");
                if (equalIndex < 0) {
                  addArea(
                      beginIndex + param.getBeginOffset(),
                      beginIndex + param.getEndOffset() + 1);
                } else {
                  addArea(
                      beginIndex + param.getBeginOffset(),
                      beginIndex + param.getBeginOffset() + equalIndex + 1);
                }
              } else {
                addArea(
                    beginIndex + param.getBeginOffset(),
                    beginIndex + param.getEndOffset() + 1);
              }
            }
          }
        }
      }
    }
  }

  /**
   * Add categories to non wiki text areas.
   * 
   * @param categories List of categories.
   */
  public void addCategories(List<PageElementCategory> categories) {
    addPageElements(categories);
  }

  /**
   * Add interwiki links to non wiki text areas.
   * 
   * @param links List of interwiki links.
   */
  public void addInterwikiLinks(List<PageElementInterwikiLink> links) {
    if (links != null) {
      for (PageElementInterwikiLink link : links) {
        int beginIndex = link.getBeginIndex();
        int endIndex = link.getEndIndex();
        if (link.getText() != null) {
          addArea(beginIndex, beginIndex + link.getTextOffset());
          addArea(endIndex - 2, endIndex);
        } else {
          addArea(beginIndex, endIndex);
        }
      }
    }
  }

  /**
   * Add language links to non wiki text areas.
   * 
   * @param links List of language links.
   */
  public void addLanguageLinks(List<PageElementLanguageLink> links) {
    addPageElements(links);
  }

  /**
   * Add ISBNs to non wiki text areas.
   * 
   * @param isbns List of ISBNs.
   */
  public void addISBN(List<PageElementISBN> isbns) {
    addPageElements(isbns);
  }

  /**
   * Add ISSNs to non wiki text areas.
   * 
   * @param issns List of ISSNs.
   */
  public void addISSN(List<PageElementISSN> issns) {
    addPageElements(issns);
  }

  /**
   * Add PMIDs to non wiki text areas.
   * 
   * @param pmids List of PMIDs.
   */
  public void addPMID(List<PageElementPMID> pmids) {
    addPageElements(pmids);
  }

  /**
   * Add templates to non wiki text areas.
   * 
   * @param templates List of templates.
   */
  public void addTemplates(List<PageElementTemplate> templates) {
    if (templates != null) {
      for (PageElementTemplate template : templates) {
        int beginIndex = template.getBeginIndex();
        int endIndex = template.getEndIndex();
        if (template.getParameterCount() > 0) {
          if (template.getParameterName(0) != null) {
            addArea(beginIndex, template.getParameterNameStartIndex(0));
          } else {
            addArea(beginIndex, template.getParameterValueStartIndex(0));
          }
          for (int numParam = 0; numParam < template.getParameterCount(); numParam++) {
            String paramName = template.getParameterName(numParam);
            if ((paramName != null) &&
                (paramName.length() > 0)) {
              addArea(
                  template.getParameterPipeIndex(numParam),
                  template.getParameterValueStartIndex(numParam));
            } else {
              int pipeIndex = template.getParameterPipeIndex(numParam);
              addArea(pipeIndex, pipeIndex + 1);
            }
          }
          addArea(endIndex - 2, endIndex);
        } else {
          addArea(beginIndex, endIndex);
        }
      }
    }
  }

  /**
   * Add functions to non wiki text areas.
   * 
   * @param functions List of functions.
   */
  public void addFunctions(List<PageElementFunction> functions) {
    if (functions != null) {
      for (PageElementFunction function : functions) {
        int beginIndex = function.getBeginIndex();
        int endIndex = function.getEndIndex();
        if (function.getParameterCount() > 1) {
          addArea(beginIndex, function.getParameterSeparatorOffset(0) + 1);
          for (int numParam = 1; numParam < function.getParameterCount(); numParam++) {
            int separatorIndex = function.getParameterSeparatorOffset(numParam);
            addArea(separatorIndex, separatorIndex + 1);
          }
          addArea(endIndex - 2, endIndex);
        } else {
          addArea(beginIndex, endIndex);
        }
      }
    }
  }

  /**
   * Add magic words to non wiki text areas.
   * 
   * @param magicWords List of magic words.
   */
  public void addMagicWords(List<PageElementMagicWord> magicWords) {
    addPageElements(magicWords);
  }

  /**
   * Add parameters to non wiki text areas.
   * 
   * @param parameters List of parameters.
   */
  public void addParameters(List<PageElementParameter> parameters) {
    addPageElements(parameters);
  }

  /**
   * Add titles to non wiki text areas.
   * 
   * @param titles Titles.
   */
  public void addTitles(List<PageElementTitle> titles) {
    if (titles != null) {
      for (PageElementTitle title : titles) {
        int beginIndex = title.getBeginIndex();
        int endIndex = title.getEndIndex();
        int after = (title.getAfterTitle() != null) ? title.getAfterTitle().length() : 0;
        addArea(beginIndex, beginIndex + title.getFirstLevel());
        addArea(endIndex - after - title.getSecondLevel(), endIndex - after);
      }
    }
  }

  /**
   * Add external links to non wiki text areas.
   * 
   * @param links Links.
   */
  public void addExternalLinks(List<PageElementExternalLink> links) {
    if (links != null) {
      for (PageElementExternalLink link : links) {
        int beginIndex = link.getBeginIndex();
        int endIndex = link.getEndIndex();
        if (link.getText() != null) {
          addArea(beginIndex, beginIndex + link.getTextOffset());
          if (link.hasSquare()) {
            addArea(endIndex - 1, endIndex);
          }
        } else {
          addArea(beginIndex, endIndex);
        }
      }
    }
  }

  /**
   * Add elements to non wiki text areas.
   * 
   * @param elements List of elements.
   */
  private void addPageElements(List<? extends PageElement> elements) {
    if (elements != null) {
      for (PageElement element : elements) {
        addArea(element.getBeginIndex(), element.getEndIndex());
      }
    }
  }

  /**
   * Add an area to the list of non wiki text areas.
   * @param beginIndex Begin index.
   * @param endIndex End index.
   */
  private void addArea(int beginIndex, int endIndex) {
    Iterator<PageElementAreas.Area> itArea = areas.iterator();
    int currentIndex = 0;
    while (itArea.hasNext()) {
      Area area = itArea.next();
      if (beginIndex <= area.endIndex) {
        if (endIndex < area.beginIndex) {
          areas.add(currentIndex, new Area(beginIndex, endIndex));
          checkAreas();
          return;
        }
        area.beginIndex = Math.min(area.beginIndex, beginIndex);
        if (endIndex <= area.endIndex) {
          return;
        }
        area.endIndex = endIndex;
        while (itArea.hasNext()) {
          Area tmpArea = itArea.next();
          if (tmpArea.beginIndex > endIndex) {
            checkAreas();
            return;
          }
          area.endIndex = Math.max(area.endIndex, tmpArea.endIndex);
          itArea.remove();
        }
        checkAreas();
        return;
      }
      currentIndex++;
    }
    areas.add(new Area(beginIndex, endIndex));
    checkAreas();
  }

  /**
   * Internal checking of the areas.
   */
  public void checkAreas() {
    int previousEnd = -1;
    for (PageElementAreas.Area area : areas) {
      if (area.beginIndex >= area.endIndex) {
        System.err.println("Error " + area);
      }
      if (previousEnd >= area.beginIndex) {
        System.err.println("Error " + area + "/" + previousEnd);
      }
      previousEnd = area.endIndex;
    }
  }

  public void printAreas(String text) {
    System.err.println("Areas " + text + " :");
    for (Area area : areas) {
      System.err.println(" " + area.getBeginIndex() + "->" + area.getEndIndex());
    }
  }

  /**
   * Utility class for memorizing an area.
   */
  public static class Area {

    /**
     * Begin index of the area.
     */
    int beginIndex;

    /**
     * End index of the area.
     */
    int endIndex;

    /**
     * @param beginIndex Begin index.
     * @param endIndex End index.
     */
    Area(int beginIndex, int endIndex) {
      this.beginIndex = beginIndex;
      this.endIndex = endIndex;
    }

    /**
     * @return Begin index.
     */
    public int getBeginIndex() {
      return beginIndex;
    }

    /**
     * @return End index.
     */
    public int getEndIndex() {
      return endIndex;
    }

    /**
     * @return Textual description.
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
      return "Area: " + beginIndex + "->" + endIndex;
    }
  }
}
