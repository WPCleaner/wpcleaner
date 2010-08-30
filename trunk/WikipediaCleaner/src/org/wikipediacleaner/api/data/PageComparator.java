/*
 *  WikipediaCleaner: A tool to help on Wikipedia maintenance tasks.
 *  Copyright (C) 2007  Nicolas Vervelle
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

import java.io.Externalizable;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.util.LinkedList;
import java.util.List;

import org.wikipediacleaner.i18n.GT;


/**
 * A comparator for pages.
 */
public abstract class PageComparator implements NamedComparator<Page>, Externalizable {

  private final static PageComparator namespaceComparator  = new NamespaceComparator();
  private final static PageComparator occurenceComparator  = new LinkOccurenceComparator();
  private final static PageComparator pageIdComparator     = new PageIdComparator();
  private final static PageComparator redirectComparator   = new RedirectComparator();
  private final static PageComparator revisionIdComparator = new RevisionIdComparator();
  private final static PageComparator templateComparator   = new TemplateComparator();
  private final static PageComparator titleComparator      = new TitleComparator();

  private static List<CompositeComparator<Page>> comparators;

  /**
   * @return Page comparator with template first (Default for disambiguation window).
   */
  public static CompositeComparator<Page> getTemplateFirstComparator() {
    List<NamedComparator<Page>> unitComparators = new LinkedList<NamedComparator<Page>>();
    unitComparators.add(templateComparator);
    unitComparators.add(redirectComparator);
    unitComparators.add(namespaceComparator);
    unitComparators.add(titleComparator);
    unitComparators.add(revisionIdComparator);
    unitComparators.add(pageIdComparator);
    unitComparators.add(occurenceComparator);
    CompositeComparator<Page> comparator = new CompositeComparator<Page>(
        "Template first", unitComparators);
    return comparator;
  }

  /**
   * @return Page comparator with revision first.
   */
  public static CompositeComparator<Page> getRevisionIdFirstComparator() {
    List<NamedComparator<Page>> unitComparators = new LinkedList<NamedComparator<Page>>();
    unitComparators.add(revisionIdComparator);
    unitComparators.add(templateComparator);
    unitComparators.add(redirectComparator);
    unitComparators.add(namespaceComparator);
    unitComparators.add(titleComparator);
    unitComparators.add(pageIdComparator);
    unitComparators.add(occurenceComparator);
    CompositeComparator<Page> comparator = new CompositeComparator<Page>(
        "Revision Id first", unitComparators);
    return comparator;
  }

  /**
   * @return Page comparator with namespace first (Default for analysis window).
   */
  public static CompositeComparator<Page> getNamespaceFirstComparator() {
    List<NamedComparator<Page>> unitComparators = new LinkedList<NamedComparator<Page>>();
    unitComparators.add(namespaceComparator);
    unitComparators.add(titleComparator);
    unitComparators.add(revisionIdComparator);
    unitComparators.add(pageIdComparator);
    unitComparators.add(templateComparator);
    unitComparators.add(redirectComparator);
    unitComparators.add(occurenceComparator);
    CompositeComparator<Page> comparator = new CompositeComparator<Page>(
        "Namespace first", unitComparators);
    return comparator;
  }
  
  /**
   * @return Page comparator with namespace first then occurence.
   */
  public static CompositeComparator<Page> getOccurenceFirstComparator() {
    List<NamedComparator<Page>> unitComparators = new LinkedList<NamedComparator<Page>>();
    unitComparators.add(namespaceComparator);
    unitComparators.add(occurenceComparator);
    unitComparators.add(titleComparator);
    unitComparators.add(revisionIdComparator);
    unitComparators.add(pageIdComparator);
    unitComparators.add(templateComparator);
    unitComparators.add(redirectComparator);
    CompositeComparator<Page> comparator = new CompositeComparator<Page>(
        "Occurence first", unitComparators);
    return comparator;
  }
  
  /**
   * @param name Comparator name.
   * @return Comparator initialized with default order.
   */
  public static CompositeComparator<Page> createComparator(String name) {
    List<NamedComparator<Page>> unitComparators = new LinkedList<NamedComparator<Page>>();
    unitComparators.add(namespaceComparator);
    unitComparators.add(titleComparator);
    unitComparators.add(revisionIdComparator);
    unitComparators.add(pageIdComparator);
    unitComparators.add(templateComparator);
    unitComparators.add(redirectComparator);
    unitComparators.add(occurenceComparator);
    CompositeComparator<Page> comparator = new CompositeComparator<Page>(
        name, unitComparators);
    return comparator;
  }

  /**
   * @return List of default comparators.
   */
  public static List<CompositeComparator<Page>> getDefaultComparators() {
    List<CompositeComparator<Page>> defComparators = new LinkedList<CompositeComparator<Page>>();
    defComparators.add(getNamespaceFirstComparator());
    defComparators.add(getTemplateFirstComparator());
    defComparators.add(getOccurenceFirstComparator());
    defComparators.add(getRevisionIdFirstComparator());
    return defComparators;
  }

  /**
   * @return List of available comparators.
   */
  public static List<CompositeComparator<Page>> getComparators() {
    if (comparators != null) {
      return comparators;
    }
    comparators = getDefaultComparators();
    return comparators;
  }

  /**
   * @param comps List of comparators.
   */
  public static void setComparators(List<CompositeComparator<Page>> comps) {
    comparators = comps;
  }

  /* (non-Javadoc)
   * @see java.io.Externalizable#writeExternal(java.io.ObjectOutput)
   */
  public void writeExternal(@SuppressWarnings("unused") ObjectOutput output) {
    //
  }

  /* (non-Javadoc)
   * @see java.io.Externalizable#readExternal(java.io.ObjectInput)
   */
  public void readExternal(@SuppressWarnings("unused") ObjectInput input) {
    //
  }

  /**
   * A comparator for Namespace. 
   */
  static class NamespaceComparator extends PageComparator {

    /**
     * Restricted to Externalisation. 
     */
    public NamespaceComparator() {
      //
    }

    /* (non-Javadoc)
     * @see org.wikipediacleaner.api.data.NamedComparator#getName()
     */
    public String getName() {
      return "Namespace";
    }

    /* (non-Javadoc)
     * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
     */
    public int compare(Page o1, Page o2) {
      if (o1.getNamespace() == null) {
        if (o2.getNamespace() == null) {
          return 0;
        }
        return 1;
      }
      if (o2.getNamespace() == null) {
        return -1;
      }
      return o1.getNamespace().compareTo(o2.getNamespace());
    }

    @Override
    public String toString() {
      return GT._("Namespace");
    }
  }

  /**
   * A comparator for Title. 
   */
  static class TitleComparator extends PageComparator {

    /**
     * Restricted to Externalisation. 
     */
    public TitleComparator() {
      //
    }

    /* (non-Javadoc)
     * @see org.wikipediacleaner.api.data.NamedComparator#getName()
     */
    public String getName() {
      return "Title";
    }

    /* (non-Javadoc)
     * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
     */
    public int compare(Page o1, Page o2) {
      if (o1.getTitle() == null) {
        if (o2.getTitle() == null) {
          return 0;
        }
        return 1;
      }
      if (o2.getTitle() == null) {
        return -1;
      }
      return o1.getTitle().compareTo(o2.getTitle());
    }

    @Override
    public String toString() {
      return GT._("Title");
    }
  }

  /**
   * A comparator for RevisionId. 
   */
  static class RevisionIdComparator extends PageComparator {

    /**
     * Restricted to Externalisation. 
     */
    public RevisionIdComparator() {
      //
    }

    /* (non-Javadoc)
     * @see org.wikipediacleaner.api.data.NamedComparator#getName()
     */
    public String getName() {
      return "RevisionId";
    }

    /* (non-Javadoc)
     * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
     */
    public int compare(Page o1, Page o2) {
      if (o1.getRevisionId() == null) {
        if (o2.getRevisionId() == null) {
          return 0;
        }
        return 1;
      }
      if (o2.getRevisionId() == null) {
        return -1;
      }
      return o2.getRevisionId().compareTo(o1.getRevisionId());
    }

    @Override
    public String toString() {
      return GT._("Revision Id");
    }
  }

  /**
   * A comparator for PageId. 
   */
  static class PageIdComparator extends PageComparator {

    /**
     * Restricted to Externalisation. 
     */
    public PageIdComparator() {
      //
    }

    /* (non-Javadoc)
     * @see org.wikipediacleaner.api.data.NamedComparator#getName()
     */
    public String getName() {
      return "PageId";
    }

    /* (non-Javadoc)
     * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
     */
    public int compare(Page o1, Page o2) {
      if (o1.getPageId() == null) {
        if (o2.getPageId() == null) {
          return 0;
        }
        return 1;
      }
      if (o2.getPageId() == null) {
        return -1;
      }
      return o1.getPageId().compareTo(o2.getPageId());
    }

    @Override
    public String toString() {
      return GT._("Page Id");
    }
  }

  /**
   * A comparator for Templates. 
   */
  static class TemplateComparator extends PageComparator {

    /**
     * Restricted to Externalisation. 
     */
    public TemplateComparator() {
      //
    }

    /* (non-Javadoc)
     * @see org.wikipediacleaner.api.data.NamedComparator#getName()
     */
    public String getName() {
      return "Template";
    }

    /* (non-Javadoc)
     * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
     */
    public int compare(Page o1, Page o2) {
      if (o1.getNamespace() == null) {
        if (o2.getNamespace() == null) {
          return 0;
        }
        return 1;
      }
      if (o2.getNamespace() == null) {
        return -1;
      }
      if (o1.getNamespace().intValue() == Namespace.TEMPLATE) {
        if (o2.getNamespace().intValue() == Namespace.TEMPLATE) {
          return 0;
        }
        return -1;
      }
      if (o2.getNamespace().intValue() == Namespace.TEMPLATE) {
        return 1;
      }
      return 0;
    }

    @Override
    public String toString() {
      return GT._("Template");
    }
  }

  /**
   * A comparator for Redirects. 
   */
  static class RedirectComparator extends PageComparator {

    /**
     * Restricted to Externalisation. 
     */
    public RedirectComparator() {
      //
    }

    /* (non-Javadoc)
     * @see org.wikipediacleaner.api.data.NamedComparator#getName()
     */
    public String getName() {
      return "Redirect";
    }

    /* (non-Javadoc)
     * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
     */
    public int compare(Page o1, Page o2) {
      if (o1.isRedirect() == o2.isRedirect()) {
        return 0; 
      }
      if (o1.isRedirect()) {
        return -1;
      }
      return 1;
    }

    @Override
    public String toString() {
      return GT._("Redirect");
    }
  }

  /**
   * A comparator for links occurence.
   */
  static class LinkOccurenceComparator extends PageComparator {

    /**
     * Restricted to Externalisation. 
     */
    public LinkOccurenceComparator() {
      //
    }

    /* (non-Javadoc)
     * @see org.wikipediacleaner.api.data.NamedComparator#getName()
     */
    public String getName() {
      return "Occurrence";
    }

    /* (non-Javadoc)
     * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
     */
    public int compare(Page o1, Page o2) {
      int c1 = o1.getCountOccurence();
      int c2 = o2.getCountOccurence();
      return c2 - c1;
    }

    @Override
    public String toString() {
      return GT._("Occurrence");
    }
  }
}
