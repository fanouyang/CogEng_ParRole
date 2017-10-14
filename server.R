library(shiny)
library(dplyr)
library(magrittr)


data<-read.csv(file="./posts_with_codes1.csv", header = TRUE)
data_part = data %>% dplyr::filter(participatory_role %in% c("Influencer","Leader","Mediator","Peripheral","Starter","Regular"))%>%
  dplyr::group_by(week, participatory_role) %>%
  dplyr::summarise(sum_IEx = sum(IEx, na.rm = TRUE),
            sum_IEl= sum(IEl, na.rm = TRUE),
            sum_IQER=sum(IQER, na.rm = TRUE),
            sum_IML=sum(IML, na.rm = TRUE),
            sum_IPA=sum(IPA, na.rm = TRUE),
            sum_GEx=sum(GEx, na.rm = TRUE),
            sum_GEl=sum(GEl, na.rm = TRUE),
            sum_GQER=sum(GQER, na.rm = TRUE),
            sum_GPA=sum(GPA, na.rm = TRUE),
            sum_GML=sum(GML, na.rm = TRUE)) %>% dplyr::group_by(week,participatory_role) %>%  dplyr::mutate(total_sum=sum(sum_IEx+sum_IEl+sum_IQER+sum_IML+sum_IPA+sum_GEx+sum_GEl+sum_GQER+sum_GPA+sum_GML))


data_part2=data_part %>% dplyr::group_by(week) %>%
  dplyr::mutate(per_IEx=round(sum_IEx/total_sum,3),per_IEl=round(sum_IEl/total_sum,3),per_IQER=round(sum_IQER/total_sum,3),per_IPA=round(sum_IPA/total_sum,3),per_IML=round(sum_IML/total_sum,3),per_GEx=round(sum_GEx/total_sum,3),per_GEl=round(sum_GEl/total_sum,3),per_GQER=round(sum_GQER/total_sum,3),per_GPA=round(sum_GPA/total_sum,3),per_GML=round(sum_GML/total_sum,3))

names(data_part2) = c("Week", "Participatory_role", "IEx", "IEl", "IQER","IPA","IML","GEx", "GEl","GQER","GPA","GML","Sum","Percentage_IEx","Percentage_IEl","Percentage_IQER","Percentage_IIPA","Percentage_IML","Percentage_GEx","Percentage_GEl","Percentage_GQER","Percentage_GPA","Percentage_GML")
data_part2$Participatory_role <- factor(data_part2$Participatory_role, levels = c("Leader","Starter","Influencer","Mediator","Regular","Peripheral"))


data_part_new=data %>% dplyr::filter(participatory_role %in% c("Influencer","Leader","Mediator","Peripheral","Starter","Regular"))%>%
  dplyr::group_by(week, participatory_role) %>%
  dplyr::summarise(IEx = sum(IEx, na.rm = TRUE),
            IEl= sum(IEl, na.rm = TRUE),
            IQER=sum(IQER, na.rm = TRUE),
            IML=sum(IML, na.rm = TRUE),
            IPA=sum(IPA, na.rm = TRUE),
            GEx=sum(GEx, na.rm = TRUE),
            GEl=sum(GEl, na.rm = TRUE),
            GQER=sum(GQER, na.rm = TRUE),
            GPA=sum(GPA, na.rm = TRUE),
            GML=sum(GML, na.rm = TRUE))


data_part_new1<- reshape2::melt(data_part_new, id.vars = c("participatory_role","week"))
colnames(data_part_new1)= c("participatory_role","week","code", "frequency")

neworder <- c("Leader","Starter","Influencer","Mediator","Regular","Peripheral")
data_part_new1 <- dplyr::arrange(mutate(data_part_new1,
                                 participatory_role=factor(participatory_role,levels=neworder)),participatory_role)



function(input, output) {

  output$plot <- renderPlot({

    if (input$code==1) {
      require(ggplot2)
      ggplot(data=data_part2,aes(x=Week, y=IEx, group=Participatory_role)) +
        geom_line(aes(linetype=Participatory_role)) +
        geom_point(aes(colour=Participatory_role), size=3, fill="white") +
        scale_y_continuous(limits = c(0, 15)) +
        ggtitle("Individual Exploration by Participatory Roles") +
        xlab("Weeks") + ylab("IEx Frequency")
    }
    else if (input$code==2){
      require(ggplot2)
      ggplot(data=data_part2,aes(x=Week, y=IEl, group=Participatory_role)) +
        geom_line(aes(linetype=Participatory_role)) +
        geom_point(aes(colour=Participatory_role), size=3, fill="white") +
        scale_y_continuous(limits = c(0, 15)) +
        ggtitle("Individual Elaboration by Participatory Roles") +
        xlab("Weeks") + ylab("IEl Frequency")
    }
    else if (input$code==3){
      require(ggplot2)
      ggplot(data=data_part2,aes(x=Week, y=IQER, group=Participatory_role)) +
        geom_line(aes(linetype=Participatory_role)) +
        geom_point(aes(colour=Participatory_role), size=3, fill="white") +
        scale_y_continuous(limits = c(0, 15)) +
        ggtitle("Individual Question Elicitation & Response by Participatory Roles") +
        xlab("Weeks") + ylab("IQER Frequency")
    }
    else if (input$code==4){
      require(ggplot2)
      ggplot(data=data_part2,aes(x=Week, y=IPA, group=Participatory_role)) +
        geom_line(aes(linetype=Participatory_role)) +
        geom_point(aes(colour=Participatory_role), size=3, fill="white") +
        scale_y_continuous(limits = c(0, 15)) +
        ggtitle("Individual Practical Application by Participatory Roles") +
        xlab("Weeks") + ylab("IPA Frequency")

    }
    else if (input$code==5){
      require(ggplot2)
      ggplot(data=data_part2,aes(x=Week, y=IML, group=Participatory_role)) +
        geom_line(aes(linetype=Participatory_role)) +
        geom_point(aes(colour=Participatory_role), size=3, fill="white") +
        scale_y_continuous(limits = c(0, 15)) +
        ggtitle("Individual MetaCognitive Learning by Participatory Roles") +
        xlab("Weeks") + ylab("IML Frequency")

    }
    else if (input$code==6){
      require(ggplot2)
      ggplot(data=data_part2, aes(x=Week, y=GEx, group=Participatory_role)) +
        geom_line(aes(linetype=Participatory_role)) +
        geom_point(aes(colour=Participatory_role), size=3, fill="white") +
        scale_y_continuous(limits = c(0, 15)) +
        ggtitle("Group Exploration by Participatory Roles") +
        xlab("Weeks") + ylab("CEx Frequency")

    }

    else if (input$code==7){
      require(ggplot2)
      ggplot(data=data_part2, aes(x=Week, y=GEl, group=Participatory_role)) +
        geom_line(aes(linetype=Participatory_role)) +
        geom_point(aes(colour=Participatory_role), size=3, fill="white") +
        scale_y_continuous(limits = c(0, 15)) +
        ggtitle("Group Elaboration by Participatory Roles") +
        xlab("Weeks") + ylab("CEl Frequency")

    }
    else if (input$code==8){
      require(ggplot2)
      ggplot(data=data_part2, aes(x=Week, y=GQER, group=Participatory_role)) +
        geom_line(aes(linetype=Participatory_role)) +
        geom_point(aes(colour=Participatory_role), size=3, fill="white") +
        scale_y_continuous(limits = c(0, 15)) +
        ggtitle("Group Question Elicitation & Response Frequency by Participatory Roles") +
        xlab("Weeks") + ylab("CQER Frequency")

    }
    else if (input$code==9){
      require(ggplot2)
      ggplot(data=data_part2,aes(x=Week, y=GPA, group=Participatory_role)) +
        geom_line(aes(linetype=Participatory_role)) +
        geom_point(aes(colour=Participatory_role), size=3, fill="white") +
        scale_y_continuous(limits = c(0, 15)) +
        ggtitle("Group Practical Application by Participatory Roles") +
        xlab("Weeks") + ylab("CPA Frequency")

    }
    else if (input$code==10){
      require(ggplot2)
      ggplot(data=data_part2, aes(x=Week, y=GML, group=Participatory_role)) +
        geom_line(aes(linetype=Participatory_role)) +
        geom_point(aes(colour=Participatory_role), size=3, fill="white") +
        scale_y_continuous(limits = c(0, 15)) +
        ggtitle("Group MetaCognitive Learning by Participatory Roles") +
        xlab("Weeks") + ylab("CML Frequency")
    }

    else {

        require(ggplot2)
        ggplot(data=data_part_new1,aes(x = code, y = frequency)) +
        geom_point(aes(color=participatory_role)) +
        facet_grid(week ~ participatory_role) +
        theme(axis.text.x = element_text(angle=60, hjust=1, size=8))

    }
  }, height = 480, width = 600)




  output$info <- renderUI({

    a("Shiny app designed by Ouyang; info about the integral analysis of social & cognitive engagement", href="https://github.com/fanouyang/sna_exampler", target="_blank")


  })


}
