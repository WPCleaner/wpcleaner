/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2012  Nicolas Vervelle
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
            PageElementTag.TAG_WIKI_NOWIKI.equals(tag.getNormalizedName())) {
          if (!tag.isEndTag() || !tag.isComplete()) {
            addArea(tag.getCompleteBeginIndex(), tag.getCompleteEndIndex());
          }
        } else {
          addArea(tag.getBeginIndex(), tag.getEndIndex());
        }
      }
    }
    checkAreas();
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
        if (image.getDescription() != null) {
          addArea(beginIndex, beginIndex + image.getDescriptionOffset());
          addArea(endIndex - 2, endIndex);
        } else {
          addArea(beginIndex, endIndex);
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
            addArea(beginIndex, template.getParameterNameOffset(0));
          } else {
            addArea(beginIndex, template.getParameterValueOffset(0));
          }
          for (int numParam = 0; numParam < template.getParameterCount(); numParam++) {
            String paramName = template.getParameterName(numParam);
            if ((paramName != null) &&
                (paramName.length() > 0)) {
              addArea(
                  template.getParameterPipeOffset(numParam),
                  template.getParameterValueOffset(numParam));
            } else {
              int pipeIndex = template.getParameterPipeOffset(numParam);
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
   * Add DEFAULTSORT to non wiki text areas.
   * 
   * @param defaultSorts List of DEFAULTSORT.
   */
  public void addDefaultSorts(List<PageElementDefaultsort> defaultSorts) {
    addPageElements(defaultSorts);
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
    checkAreas();
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
            return;
          }
          area.endIndex = Math.max(area.endIndex, tmpArea.endIndex);
          itArea.remove();
        }
        return;
      }
      currentIndex++;
    }
    areas.add(new Area(beginIndex, endIndex));
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
