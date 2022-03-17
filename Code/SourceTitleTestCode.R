###########################################################################
#
# Research trends in forensic science: Fibre and Databases
#
# Leverhulme Research Centre for Forensic Science

# Virginie Galais,Holly Fleming, Hervé Ménard and Niamh Nic Daéid

# Website: https://github.com/LRCFS/
# Contact: lrc@dundee.ac.uk
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
###########################################################################
#
# This code is for Figure X
#
###########################################################################

#Clear all lists from memory to avoid unintentional errors

rm(list = ls())

library(ggpubr)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(reshape2)

dat = read.delim('Table2.csv', sep = ",")
ifsms.dat = dat[,1:4]
#scopus.dat = dat[,c(1,5:6)]

ifsms.dat$Journal <- factor(ifsms.dat$Journal)
# take difference of reference counts
# and make long
ifsms.l <- gather(ifsms.dat, Database, Count, Scopus:WOS, factor_key=TRUE)
  # mutate(IFSMS2 = IFSMS - IFSMSandScopus) %>% 
  # select(-IFSMS) %>% 
  

# plot as stacked barplot
plt1 = ggplot(ifsms.l, aes(x = Journal, y = Count, fill = Database)) + 
  geom_col() + 
  coord_flip() + 
  scale_fill_manual(labels = c('Scopus only', 'Scopus & WOS', 'WOS only'), values = brewer.pal(4, 'Blues')[1:3]) + 
#  ggtitle('Title') + 
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        legend.text=element_text(size=16),
        legend.title=element_text(size=16),
        )
show(plt1)

ggsave("Results/Figure5.tiff", width = unit(14.5, 'in'), height = unit(6, 'in'), dpi=300)

print("Processing complete. Please check 'Results' folder for output")