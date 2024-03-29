# Variable Description

## group
**Categorical variable**  
1: Control Group  
2: Treatment Group (Female)  
3: Treatment Group (Female + Upvote)  
4: Treatment Group (Male)  
5: Treatment Group (Male + Upvote)  

## treated
**Binary variable**  
0: Control Group (Group 1)  
1: Treatment Group (Group 2, 3, 4, 5)

## treated_male
**Binary variable**  
0: Control Group + Female-treated group (Group 1, 2, 3)  
1: Male-treated group (Group 4, 5)

## treated_upvotes
**Binary variable**  
0: Control Group + Few-upvotes group (Group 1, 2, 4)  
1: Many-upvotes group (Group 3, 5)

## id
Respondent index, '응답코드' in the raw data

## male
**Binary variable**  
0: A respondent is female  
1: A respondent is male

## age
**Continuous variable**  
A respondent's age (year)

## job_raw
Respondent's job index, '직업' in the raw data

## region_raw
Respondent's region index, '지역' in the raw data

## poli_conserv
A respondent's political orientation (conservativeness)  
[Treated as missing] 잘 모르겠음  
1: 매우 진보적  
2: 다소 진보적  
3: 진보도 보수도 아님  
4: 다소 보수적  
5: 매우 보수적  

## poli_conserv_fill
A respondent's political orientation (conservativeness)  
1: 매우 진보적  
2: 다소 진보적  
3: 진보도 보수도 아님 (or 잘 모르겠음)  
4: 다소 보수적  
5: 매우 보수적  

## party_raw
**Categorical variable**  
Party that a respondent supports most
Q. 귀하께서 가장 지지하는 정당은 무엇입니까?  
0: 지지정당 없음  
1: 더불어민주당    
2: 미래통합당  
3: 정의당  
4: 국민의당  
5: 열린민주당  
6: 기타정당(직접입력)  

## party_liberal
Whether or not a respondent's most supporting party is liberal   
0: No (Party 0, 2, 4, 6)   
1: Yes (Party 1, 3, 5)    

## party_conserv
Whether or not a respondent's most supporting party is conservative   
0: No (Party 0, 1, 3, 4, 5, 6)   
1: Yes (Party 2)   

## metoo_attitude
**Continuous variable (5-point Likert scale)**  
A respondent's attitude toward the Me Too movement   
Q. 귀하께서는 미투 운동에 대해 어떻게 생각하십니까?    
1: 매우 부정적  
2: 다소 부정적  
3: 중립적   
4: 다소 긍정적   
5: 매우 긍정적  
Note. In the raw data, the value was coded reversely (i.e., from 1: Very positive to 5: Very negative).

## comm_attitude
**Continuous variable (5-point Likert scale)**  
A respondent's attitude toward the comment  
Q. 아래 그림은 앞서 영상에 달렸던 댓글입니다. 위 댓글에 대한 귀하의 생각은 어떻습니까?  
1: 매우 부정적  
2: 다소 부정적  
3: 중립적   
4: 다소 긍정적   
5: 매우 긍정적  
Note. In the raw data, the value was coded reversely (i.e., from 1: Very positive to 5: Very negative).

## comm_hate
**Continuous variable (5-point Likert scale)**  
The extent to which a respondent perceives the comment as hate speech     
Q. 아래 그림은 앞서 영상에 달렸던 댓글입니다. 귀하께서는 이 댓글이 혐오 발언이라고 생각하십니까?       
1: 전혀 그렇지 않다.  
2: 그렇지 않다.  
3: 보통이다.   
4: 그렇다.   
5: 매우 그렇다.  
Note. In the raw data, the value was coded reversely (i.e., from 1: Very likely to 5: Very unlikely).

## comm_liking
**Continuous variable (5-point Likert scale)**  
A respondent's intention to click 'like' on the comment   
Q. 아래 그림은 앞서 영상에 달렸던 댓글입니다. 귀하께서는 위 댓글에 '좋아요'(또는 엄지 버튼)를 누를 의향이 있으십니까?     
1: 전혀 그렇지 않다.  
2: 그렇지 않다.  
3: 보통이다.   
4: 그렇다.   
5: 매우 그렇다.  
Note. In the raw data, the value was coded reversely (i.e., from 1: Very likely to 5: Very unlikely).

## comm_user_attitude
**Continuous variable (5-point Likert scale)**  
A respondent's attitude toward the commenter  
Q. 아래 그림은 앞서 영상에 달렸던 댓글입니다. 위 댓글 작성자에 대한 귀하의 생각은 어떻습니까?  
1: 매우 부정적  
2: 다소 부정적  
3: 중립적   
4: 다소 긍정적   
5: 매우 긍정적  
Note. In the raw data, the value was coded reversely (i.e., from 1: Very positive to 5: Very negative).

## comm_report
**Continuous variable (5-point Likert scale)**  
A respondent's intention to report the comment   
Q. 아래 그림은 앞서 영상에 달렸던 댓글입니다. 귀하께서는 유튜브의 신고 기능을 통해, 위 댓글을 신고할 의향이 있으십니까?   
1: 전혀 그렇지 않다.  
2: 그렇지 않다.  
3: 보통이다.   
4: 그렇다.   
5: 매우 그렇다.  
Note. In the raw data, the value was coded reversely (i.e., from 1: Very likely to 5: Very unlikely).

## reply_attitude
**Continuous variable (5-point Likert scale)**  
A respondent's attitude toward the reply  
Q. 아래 그림은 앞서 영상의 댓글에 달린 답글입니다. 위 답글에 대한 귀하의 생각은 어떻습니까?  
1: 매우 부정적  
2: 다소 부정적  
3: 중립적   
4: 다소 긍정적   
5: 매우 긍정적  
Note. In the raw data, the value was coded reversely (i.e., from 1: Very positive to 5: Very negative).

## reply_upvotes
**Continuous variable (5-point Likert scale)**  
Whether a respondent thinks that the reply received many upvotes  
Q. 아래 그림은 앞서 영상의 댓글에 달린 답글입니다. 다음 주장에 대해 얼마나 동의하십니까? "위 답글은 많은 수의 '좋아요'(또는 엄지 버튼)를 받았다."    
1: 매우 반대한다.  
2: 반대한다.  
3: 동의도 반대도 아니다.   
4: 동의한다.   
5: 매우 동의한다.  
Note. In the raw data, the value was coded reversely (i.e., from 1: Strongly agree to 5: Strongly disagree).

## reply_maleportion
**Continuous variable (5-point Likert scale)**  
A respondent's perceived proportion of male users among those who clicked 'like' on the reply   
Q. 아래 그림은 앞서 영상의 댓글에 달린 답글입니다. 위 답글에 '좋아요'(또는 엄지 버튼)를 누른 사용자들의 성별은 대체로 어떨 것으로 예상하십니까?    
1: 여자가 대부분일 것이다.  
2: 여자가 좀 더 많을 것이다.  
3: 남자와 여자의 수가 비슷할 것이다.  
4: 남자가 좀 더 많을 것이다.  
5: 남자가 대부분일 것이다.
Note. In the raw data, the data was coded reversely.

## reply_socialnorm
**Continuous variable (5-point Likert scale)**  
A respondent's subjective norm regarding the reply   
Q. 아래 그림은 앞서 영상의 댓글에 달린 답글입니다. 다음 주장에 대해 얼마나 동의하십니까? "나에게 중요한 사람들은 대부분 내가 위 답글을 긍정적으로 생각하기를 기대할 것이다."    
1: 매우 반대한다.  
2: 반대한다.  
3: 동의도 반대도 아니다.   
4: 동의한다.   
5: 매우 동의한다.  
Note. In the raw data, the value was coded reversely (i.e., from 1: Strongly agree to 5: Strongly disagree).

## reply_liking
**Continuous variable (5-point Likert scale)**  
A respondent's intention to click 'like' on the reply   
Q. 아래 그림은 앞서 영상의 댓글에 달린 답글입니다. 귀하께서는 위 답글에 '좋아요'(또는 엄지 버튼)를 누를 의향이 있으십니까?     
1: 전혀 그렇지 않다.  
2: 그렇지 않다.  
3: 보통이다.   
4: 그렇다.   
5: 매우 그렇다.  
Note. In the raw data, the value was coded reversely (i.e., from 1: Very likely to 5: Very unlikely).

## reply_user_attitude
**Continuous variable (5-point Likert scale)**  
A respondent's attitude toward the replier  
Q. 아래 그림은 앞서 영상의 댓글에 달린 답글입니다. 위 답글 작성자에 대한 귀하의 생각은 어떻습니까?
1: 매우 부정적  
2: 다소 부정적  
3: 중립적   
4: 다소 긍정적   
5: 매우 긍정적  
Note. In the raw data, the value was coded reversely (i.e., from 1: Very positive to 5: Very negative).

## reply_user_realname
**Binary Variable**  
Q. 아래 그림은 앞서 영상의 댓글에 달린 답글입니다. 귀하께서는 위 답글 작성자의 이름이 작성자 본인의 실제 이름이라고 생각하십니까?  
0: 아니다.
1: 그렇다.  
Note. In the raw data, '그렇다' indicated 1, and '아니다' indicated 2.

## reply_user_male
**Binary variable**  
Given the question of "아래 그림은 앞서 영상의 댓글에 달린 답글입니다. 위 답글 작성자의 성별은 무엇이라고 생각하십니까?" and three options "남자", "여자", "알 수 없음", a respondent's answer was   
0: "여자" or "알 수 없음"  
1: "남자" 

## reply_user_female
**Binary variable**  
Given the question of "아래 그림은 앞서 영상의 댓글에 달린 답글입니다. 위 답글 작성자의 성별은 무엇이라고 생각하십니까?" and three options "남자", "여자", "알 수 없음", a respondent's answer was   
0: "남자" or "알 수 없음"  
1: "여자" 

## reply_user_dontknow
**Binary variable**  
Given the question of "아래 그림은 앞서 영상의 댓글에 달린 답글입니다. 위 답글 작성자의 성별은 무엇이라고 생각하십니까?" and three options "남자", "여자", "알 수 없음", a respondent's answer was   
0: "남자" or "여자"  
1: "알 수 없음" 

## reply_report
**Continuous variable (5-point Likert scale)**  
A respondent's intention to report the reply   
Q. 아래 그림은 앞서 영상의 댓글에 달린 답글입니다. 귀하께서는 유튜브의 신고 기능을 통해, 위 답글을 신고할 의향이 있으십니까?     
1: 전혀 그렇지 않다.  
2: 그렇지 않다.  
3: 보통이다.   
4: 그렇다.   
5: 매우 그렇다.  
Note. In the raw data, the value was coded reversely (i.e., from 1: Very likely to 5: Very unlikely).

## youtube_attitude
**Continuous variable (5-point Likert scale)**  
A respondent's attitude toward YouTube   
Q. 귀하께서는 유튜브에 대해 어떻게 생각하십니까?  
1: 매우 부정적  
2: 다소 부정적  
3: 중립적   
4: 다소 긍정적   
5: 매우 긍정적  
Note. In the raw data, the value was coded reversely (i.e., from 1: Very positive to 5: Very negative).

## youtube_selfcorrection
**Continuous variable (5-point Likert scale)**  
The extent to which a respondent belives that users can self-correct YouTube commenting culture.   
Q. 다음 주장에 대해 얼마나 동의하십니까? "유튜브의 댓글 문화는 사용자들 스스로 개선해나갈 수 있다."  
1: 매우 반대한다.  
2: 반대한다.  
3: 동의도 반대도 아니다.   
4: 동의한다.   
5: 매우 동의한다.  
Note. In the raw data, the value was coded reversely (i.e., from 1: Strongly agree to 5: Strongly disagree).

## youtube_regulation
**Continuous variable (5-point Likert scale)**  
A respondent's attitude toward the internal regulation (YouTube policy on hate speech)   
Q. 다음 주장에 대해 얼마나 동의하십니까? "유튜브가 직접 사용자들의 혐오 발언을 규제해야 한다."  
1: 매우 반대한다.  
2: 반대한다.  
3: 동의도 반대도 아니다.   
4: 동의한다.   
5: 매우 동의한다.  
Note. In the raw data, the value was coded reversely (i.e., from 1: Strongly agree to 5: Strongly disagree).

## law_individual
**Continuous variable (5-point Likert scale)**  
A respondent's attitude toward the external regulation (government policy on individuals)   
Q. 다음의 내용을 잘 읽고 질문에 답해 주시기 바랍니다. 벨기에에서는 성을 토대로 경멸적인 표현을 하거나 열등한 것으로 간주하는 모든 행동을 성차별로 규정하여, 이를 어기면 최대 징역 1년 또는 벌금 1,000유로(약 133만원)에 처하는 법안이 2014년에 통과되었습니다. 실제로 벨기에에서는 2018년 3월, 여성 경찰관을 향해 공공장소에서 성차별적인 발언을 한 20대 남성에게 벌금형이 선고되었습니다. 귀하께서는 위의 법안이 대한민국에도 필요하다는 주장에 동의하십니까?  
1: 매우 반대한다.  
2: 반대한다.  
3: 동의도 반대도 아니다.   
4: 동의한다.   
5: 매우 동의한다.  
Note. In the raw data, the value was coded reversely (i.e., from 1: Strongly agree to 5: Strongly disagree).

## law_company
**Continuous variable (5-point Likert scale)**  
A respondent's attitude toward the external regulation (government policy on platforms)   
Q. 다음의 내용을 잘 읽고 질문에 답해 주시기 바랍니다. 프랑스에서는 2019년 온라인 플랫폼과 검색 엔진 운영자(예: 페이스북, 구글)에게 인종 및 종교 등에 대한 혐오 발언을 24시간 이내에 삭제하도록 강제하는 법안을 의결하였습니다. 관련 콘텐츠를 24시간 내에 삭제하지 않은 기업은 최대 125만 유로(약 17억 원)의 벌금을 물게 됩니다. 귀하께서는 위의 법안이 대한민국에도 필요하다는 주장에 동의하십니까?  
1: 매우 반대한다.  
2: 반대한다.  
3: 동의도 반대도 아니다.   
4: 동의한다.   
5: 매우 동의한다.  
Note. In the raw data, the value was coded reversely (i.e., from 1: Strongly agree to 5: Strongly disagree).

## education
**Ordinal variable**  
A respondent's education level  
Q. 귀하의 최종 학력은 어떻게 되십니까?   
1: 중졸 이하  
2: 고등학교 졸업  
3: 전문대 재학  
4: 전문대 졸업  
5: 대학교 재학  
6: 대학교 졸업  
7: 대학원 재학  
8: 대학원 졸업  

## income
**Ordinal variable**  
A respondent's household income level (by million KRW)  
Q. 귀하의 가구 구성원 전체의 소득은 어떻게 되십니까?  
1: 월 99만원 이하  
2: 월 100만원 - 199만원  
(...)  
10: 월 900만원 - 999만원  
11: 월 1000만원 이상
