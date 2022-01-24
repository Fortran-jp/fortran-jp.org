# "Modern Fortran"が含まれる論文　2021年編（2021年3月26日時点）

「modern fortran」と言う単語が含まれているページをGoogleScholarで検索。「モダンFortran」は無し。

* https://arc.aiaa.org/doi/abs/10.2514/1.J059505

Novel Expression-Template-Based Automatic Differentiation of Fortran Codes for Aerodynamic Optimization
Reza Djeddi and Kivanc Ekici
本研究では、離散アドジョイントに基づくFortranコードの自動微分のためのエレガントでロバストなアプローチを提案する。この手法は、C/C++コードではコンパイル時に利用可能なメタプログラミング・パラダイムが存在しないFortranプログラミング言語のギャップを埋めることを目的としている。具体的には、本研究では、アジョイントベースの自動微分（AD）の計算効率を向上させつつ、メモリフットプリントを大幅に削減することができる、Fortranプログラミング言語では初の実装となる式ベースのテープアプローチを使用しています。提案された式例ベースのアプローチは、当社の社内用ADツールボックスに組み込まれています。このツールボックスは、現在、定点演算子オーバーロード型のアジョイント感度解析を使用する、文献上唯一のFortranベースのツールです。このツールボックスは、現在のところ、定点演算子オーバーロード型のアジョイント感度解析を使用する唯一のFortranベースのツールです。このツールボックスの改良版は、UNPAC-DOFと呼ばれるロバスト設計最適化フレームワーク（DOF）のために、社内の非構造化並列圧縮性（UNPAC）フローソルバーと結合されます。提案された手法と結果として得られたフレームワークの効率性とロバスト性を、翼型と翼の形状に適用された空力形状最適化問題でテストしました。

* https://www.sciencedirect.com/science/article/pii/S235271102030368X

FLUBIO—An unstructured, parallel, finite-volume based Navier–Stokes and convection–diffusion like equations solver for teaching and research purposes
E.Alinovi J.Guerrero
FLUBIOは、ナビエ・ストークス方程式や対流拡散方程式などを解くための、構造化されていない有限体積ベースの並列ソルバーです。このソルバーは、最新のFortran（2003+規格）を用いて記述されており、オブジェクト指向で、理解と修正が容易なように構成されています。このソルバーは、学生、研究者、個人ユーザー、実務者を対象としており、最新のCFDソリューション手法や離散化技術の背景にある一般的な理論を理解するのに役立ちます。また、このソルバーは、流体力学の分野で見られる産業上および学術上の問題に対処するのに十分な汎用性と能力を備えています。

* https://gmd.copernicus.org/preprints/gmd-2021-22/

fv3gfs-wrapper: a Python wrapper of the FV3GFS atmospheric model
Jeremy McGibbon et al.
地球物理学分野のシミュレーションソフトウェアは、厳しい性能要件を満たす必要があるため、伝統的にFortranやC++で記述されています。fv3gfs-wrapperは、Fortranで記述されたNOAAのFV3GFSグローバル大気モデルをPythonでラッピングしたオープンソースのソフトウェアです。ラッパーは、Fortranのメインループを進行させ、Fortranモデルから状態を取得または設定するためのシンプルなインターフェースを提供します。これらのインターフェースにより，モデルの動作を変更したり，オンライン解析コードを導入したり，モデルの状態を保存したり，フォーシングをクラウドストレージとの間で直接読み取ったりするなど，幅広いユースケースが可能になります．モデルのパフォーマンスは，状態をモデルの内外にコピーするルーチンを使用しない限り，完全にコンパイルされたFortranモデルと同じです．このコピーオーバーヘッドは、性能の許容範囲内であり、Fortranソースコードを修正することで回避することができます。このラッピング手法の概要は、他のFortranモデルにも同様に適用することができ、より生産性の高い科学的ワークフローを実現します。

* https://arc.aiaa.org/doi/abs/10.2514/6.2021-0143

Optimisation of a Finite-Volume Test-bench Code for Highly Parallel Architectures
Laurence Kedward and Christian B. Allen
著者らは以前、GPUなどの高度な並列コンピューティング・アーキテクチャ上での、有限量のテストベンチ・コードのコード構造と最適化について研究しました。予備的な結果として、2つの高性能コンシューマーGPUを使用した場合、第9世代のインテルCPUの16スレッドと比較して、全体で18倍から24倍のスピードアップが見られました。ここでは、典型的な有限体積カーネルの高度なメモリ拘束性に注目して、さらなる結果を発表します。メモリ帯域幅が制限されている状況下で性能を向上させるために、共有メモリを使用したカーネルマージと、混成精度陰解法ソルバーの2つの手法を紹介します。カーネルマージコードの最適化は，GPUアーキテクチャの高速なオンチップ共有メモリを利用することで，演算強度を高め，カーネル起動のオーバーヘッドを低減します．キャッシュの利用効率が向上したことにより，GPUとIntel CPUの両方で大幅な性能向上が見られました．単純な混合精度の陰解法を用いた結果、線形システムを単精度で解くことができる一方で、非線形フロー残差については完全な倍精度を維持できることが確認されました。さらに，GPUアーキテクチャーで線形システムに単精度を使用すると，必要なメモリバンド幅が減少するため，2倍近いスピードアップが達成されました．

* https://www.sciencedirect.com/science/article/pii/S2352711020303605

XMHFL: Software for calculating excited and ionized states of molecules by X-ray
AntonKasprzhitskiiab VictorYavnaa
XMHFLは、1つ以上の空孔を持つ分子系の基底状態、励起状態、イオン化状態を記述するSingle Center Expansion (SCE)法を実装したコンピュータプログラムです。シミュレーションのメカニズムは、3点差Numerov法とThomasアルゴリズムの組み合わせに基づいています。XMHFLソルバーのモジュラーアーキテクチャは、生成されたコードを新しい手法やアルゴリズムの開発のベースとして使用することを可能にし、また、X線分子分光法の研究や教育プラットフォームとしてのトレーニングのための分子特性計算の可能性を高めます。

* https://www.sciencedirect.com/science/article/pii/S0022407321000170

How much is enough? The convergence of finite sample scattering properties to those of infinite media
Antti Penttiläa Karri Muinonenaf
我々は、粒子の雲の散乱特性を研究しています。粒子は、入射波長に近い大きさの球形で、高いアルベドを持ち、20％の体積密度でランダムに充填されている。マクスウェル方程式を解く数値的に厳密な方法と、放射伝達近似法の両方を用いて、雲の散乱特性は、系内の約1000万個の粒子の後に収束することを示した。その後、この系の後方散乱特性は、巨視的で実質的に無限の系の特性を推定することになる。

* https://www.sciencedirect.com/science/article/pii/S0022407321001114

Impacts of laser beam divergence on lidar multiple scattering polarization returns from water clouds
Zhen Wanga Haiyang Gaoa
理論的には、レーザービームの発散は、レーザー照射を拡散させることで多重散乱光を再分配し、ライダの偏光測定を変化させることができる。ライダの偏光信号に対するレーザ発散の多重散乱効果を調べるために、モンテカルロ偏光放射伝達モデルMSCARTを適用して、一様な水雲からのライダストークスベクトル信号を、レーザ発散を考慮した場合と考慮しない場合でシミュレーションした。比較分析の結果、複数の散乱領域の大きさがほぼ同じであるライダー受信機のフットプリントでは、レーザー発散は地上設置型のライダー信号にはほとんど影響を与えないが、宇宙設置型のライダー信号には非常に大きく複雑な偏光効果を与えることがわかった。レーザー発散の増加は、発散がFOVより大きい場合には、スペースボーン・ライダーの多重散乱偏光を大幅に強化し、一方で、発散がFOVより小さい場合には、わずかに弱めることができる。最も弱い多重散乱偏光は、FOVと等しい発散度で発生する。さらに，レーザー発散は，位相行列の非対角要素に対する宇宙機ライダのFOV分解偏光測定の感度を大幅に低下させ，異なる受信極角での測定が，ほぼ同じ散乱角での位相行列の対角要素にしか反応しなくなる。このように、宇宙borne MFOVおよびCCD偏光ライダーは、単一FOVライダーに比べて、位相行列に関するより多くの情報を提供できない可能性があります。

* https://aip.scitation.org/doi/abs/10.1063/5.0036926

Computational study of the processes of erosive interaction of rain particles with the surface of the fairing at the stage of spacecraft launching
A. A. Ivankov and A. L. Melkisheva
宇宙船が実用軌道に乗ると、対向する大気ガスの流れによる熱的・力的な影響を受けます。これらの影響を防ぐためにヘッドフェアリングが使用されていますが、このヘッドフェアリングの熱保護機能は、雨食の影響を考慮せずに開発されており、雨食を通過しない軌道に適しています。打ち上げが天候に左右されないようにするためには、フェアリングの熱保護コートの厚さを適宜増やし、侵食による追加のアブレーションを考慮した厚さの余裕を持たせる必要がある。この論文では、大気圏内の宇宙船発射場におけるヘッドフェアリングの熱保護に対する雨の浸食の影響を計算する方法を紹介しています。バイコヌール宇宙基地周辺の空気湿度のモデルを考慮している。計算結果は、宇宙物体の研究を目的とした有望な宇宙船の打ち上げの軌道と頭部フェアリングの熱保護設計について示されている。

* https://arc.aiaa.org/doi/abs/10.2514/6.2021-0159

Implicit Thermochemical Nonequilibrium Flow Simulations on Unstructured Grids using GPUs
Gabriel Nastac, Aaron Walden, Eric J. Nielsen and Kader Frendi
熱化学非平衡流体シミュレーション機能は，NASAの非構造格子型数値流体力学ソルバーFUN3Dにおいて，中央演算処理装置（CPU）向けに実装，検証，妥当性の確認が行われている．しかし、エクサスケールクラスのハイパフォーマンスコンピューティングシステムの多くは、高い処理能力とエネルギー効率を実現するためにGPU（Graphics Processing Unit）アーキテクチャに依存しているため、これらのシステムを効果的に利用できないCPUベースの科学技術計算ソフトウェアを更新する必要があります。本研究では、NVIDIA Tesla GPUを対象としたFUN3Dの熱化学非平衡流体シミュレーション機能のCUDA C++による実装を紹介します。移植と最適化戦略の概要を説明し、最近の他のアーキテクチャとの性能比較を行います。また、数千のGPUへのスケーリングを行い、数百万のCPUコアに匹敵する計算性能を実現しました。この実装により、大気圏突入、極超音速、燃焼などの多くの用途において、熱化学的な非平衡流の効率的で高忠実なスケール解決型シミュレーションが可能になります。

* https://vtechworks.lib.vt.edu/handle/10919/102073

CPU/GPU Code Acceleration on Heterogeneous Systems and Code Verification for CFD Applications
Xue, Weicheng
CFD（Computational Fluid Dynamics）は、流体問題を解決するための数値計算手法で、通常は大量の計算を必要とします。大規模なCFD問題は、小さなサブ問題に分解され、それらは離散的なメモリ位置に格納され、多数の計算ユニットによって加速されます。コードの高速化に加えて，コードやアルゴリズムが正しく実装されているかどうかを確認することも重要であり，これをコード検証と呼びます．この論文では，CFDコードの高速化と，乱流モデルの実装におけるコード検証に焦点を当てています。この論文では，GPUが高い計算能力と高いメモリ帯域幅を備えていることを考慮し，複数のGPU（Graphic Processing Unit）を使用して2つのCFDコードを高速化しています．さまざまな並列コンピューティングシステム上でCFDコードの性能を向上させるために，さまざまな最適化手法を開発・適用しました．特に、複数のGPUを使用した場合、プログラムの実行時間を大幅に短縮することができます。また，研究用CFDコードの正しさを検証するために，いくつかのNASA製CFDコードとのコード間比較や，製造されたソリューションの手法を利用しています．

* https://www.sciencedirect.com/science/article/pii/S089684462030293X

Investigation of post-injection strategies for diesel engine Catalyst Heating Operation using a vapor-liquid-equilibrium-based spray model
FedericoPeriniaRolf DeneysReitzac
多次元エンジンシミュレーションの多くは、非平衡スプレーダイナミクス（霧化、衝突、気化）の解法に多くの時間を費やしています。しかし、その精度はグリッド依存性が大きいために制限されており、大規模なキャリブレーションが必要となります。これは、一般的なモデルの検証範囲から離れた低温高圧下で発生するコールドスタート時のディーゼル燃料のポスト噴射をモデル化する際には非常に重要です。一方で，ミクロンスケールの噴霧現象を解明するには，オイラー型の多相計算では対応できません．本研究では，ディーゼル触媒の加熱運転戦略をシミュレーショ ンするために，改良された相平衡ベースの手法を導入し，その評価を行い ました．Yue と Reitz [1] のモデルをベースにした相平衡ソルバーを実装しました．エンジニアリングサイズのエンジングリッドを使用して完全な多相 CFD ソルバーを使用し，燃料噴射は標準的なラグランジュ法によるパーセルアプローチを使用してモデル化しました．液体パーセルからの質量とエネルギーは，平衡ベースの液体ジェットモデルに従ってオイラー型の多相混合物に放出されます．正確な化学動力学メカニズムから得られるような大規模な実在気体混合物を扱うために，改良された相平衡ソルバーが開発されました．また，液体ジェットモデルを改良し，オイラー型ソルバーへの運動量の伝達が物理的なスプレージェット構造をよりよく再現できるようにした．また，液体／蒸気の浸透予測を検証した結果，このモデルは非常に限られた調整で正確な結果を得ることができ，少数の校正定数に対する感度も低いことが分かりました．また，ディーゼル触媒を加熱する運転方法をシリンダー内でシミュレーションしたところ，短時間の過渡的な噴射パルスと低温が存在する場合には，スプレー構造を捉えることが最も重要であることが分かりました．さらに，従来のスプレーモデルでは膨張行程での液体の浸透性の増加を捉えることができませんでしたが，EP モデルでは，噴射後のスプレー構造と着火性をより正確に予測することができました．最後に，EPモデルの唯一の重要な校正定数であるCliqは，運動量の伝達には影響しないが，局所的なエネルギー伝達を通じて局所的な電荷の冷却分布を変化させるため，追加研究の候補となっている．この結果から、高圧ディーゼルスプレーのエンジニアリングシミュレーションにおいて、非平衡スプレープロセスを解決する必要がないことが確認されました。

* https://dl.acm.org/doi/abs/10.1145/3432261.3432265

Spectral Element Simulations on the NEC SX-Aurora TSUBASA
Niclas Jansson
近年、ハイパフォーマンスコンピューティングの分野では、よりヘテロジニアスなアーキテクチャへの移行が進んでおり、アプリケーション開発者は、多様なプラットフォーム間で良好なパフォーマンスを確保するという課題に直面しています。本論文では、スペクトル要素コードNek5000を、最新のベクトルアーキテクチャSX-Aurora TSUBASAに移植する作業について紹介します。Nek5000のミニアプリ「Nekbone」を用いて、主要なカーネルに適切なループ変換を施し、より良いベクトル化を可能にすることで、ベースラインの性能を6倍に向上させました。この新しい変換を用いて、計算量の多い行列ベクトル乗算および行列マトリクス乗算の主要なカーネルが、SX-Auroraコアのピーク性能の半分近くになることを実証しました。また，行列なしのスペクトル要素を効率的に定式化するための重要なカーネルであるギャザースキャッター演算にも取り組んでいます。Nek5000のギャザースキャッターライブラリにメッシュトポロジーを考慮した新しい実装を導入し、SX-Auroraのハードウェアギャザースキャッター命令を利用してベクトル化を改善することで、最大116%の性能向上を実現しました。この実装の詳細については、シングルノードでの性能と、複数のSX-Auroraカードで実行した場合の強力なスケーラビリティ特性の両方を比較したパフォーマンススタディとともに説明しています。

* https://www.sciencedirect.com/science/article/pii/S0306454921000372

Numerical comparison of mathematical and computational models for the simulation of stochastic neutron kinetics problems
T.L.GordonM.D.Eaton
本論文は，低外乱（外因性または固定）中性子源アプリケーションにおける中性子の確率的挙動をモデル化することができる5つの数学モデルの数値比較に関するものである。これらのモデルには、アナログモンテカルロ(AMC),前方確率バランス方程式(FPB),前方確率バランス方程式の生成関数形式(FGF),後方確率バランス方程式の生成関数形式(Pál-Bell),明示的及び暗黙的なEuler-Maruyama離散化スキームを用いた伊藤計劃モデルがある。その結果、生存確率、絶滅確率、中性子集団の平均値と標準偏差、中性子集団の累積分布関数などが比較された。最も計算負荷の少ない数理モデルは，Pál-Bell方程式を用いたものであり，本研究の他の手法と比較して，平均して4桁の計算時間で済むことが判明した。AMCモデルとFPBモデルの精度は，モデルの計算効率と強く関連していることがわかった．最大許容中性子数に近づくと、モデルの計算効率は著しく低下する。明示的および暗黙的なオイラー・丸山離散化スキームを用いた伊藤計劃法は、非常に低い中性子数のモデル化には適していないことがわかった。しかし、より多くの中性子を含む系では、伊藤計劃法を用いて改善された結果が得られている。

* https://arc.aiaa.org/doi/abs/10.2514/6.2021-1742

Code Verification for 2D Unsteady Flows in SENSEI
Weicheng Xue, Hongyu Wang and Christopher J. Roy
圧縮性CFDコードにおける非定常流れのコード検証には，通常，製造された解法またはソース項を追加した正確な非圧縮性解法を使用する必要があります．空間と時間のオーダーを組み合わせた解析では、誤った解析を避けるために、空間の離散化誤差は時間の誤差と同程度のオーダーにする必要があります。非定常流の場合は、空間的および時間的な間隔の両方について系統的な精密化を行い、観測された全体的な精度の正しい順序を決定する必要があります。明示的な時間行進法は、安定性の制約から暗黙的な時間行進法に比べて一般的に小さな時間ステップサイズを必要とするため、本研究では安定性の制約を緩和するために、Singly-Diagonally Implicit Runge-Kutta multi-stage スキームや3点後退スキームなどの複数の暗黙的なスキームを使用しています。

* https://onlinelibrary.wiley.com/doi/abs/10.1002/jcc.26450

New parallel computing algorithm of molecular dynamics for extremely huge scale biological systems
Jaewoon Jung
本論文では、100,000以上のCPUコアでセルラースケールの分子動力学(MD)シミュレーションを行うために、GENESISソフトウェアの高性能なエクストリームスケール分子動力学(MD)アルゴリズムを取り上げます。その内容は、(1)ARM CPUアーキテクチャで性能を最大化する実空間非結合相互作用の新アルゴリズム、(2)通信コストを最小化する相互空間非結合相互作用、(3)大きな時間ステップを可能にする正確な温度・圧力評価、(4)極めて巨大なシステムのMDシミュレーションのための効果的な並列ファイル入出力（I/O）、などである。16億個の原子を含む最大の系を、富嶽スパコンで8.30ns/dayの性能でMDを用いてシミュレーションした。これにより、MDシミュレーションの利用可能なサイズと時間が拡張され、生体細胞内の生体高分子の未解決の問題に答えることができます。

* https://arxiv.org/abs/2101.04722

Atomic line radiative transfer with MCFOST I. Code description and benchmarking
B. Tessore, C. Pinte, J. Bouvier, F. Ménard
目的 多階層原子システムのための新しい非局所熱力学的平衡放射伝達ソルバーであるMCFOST-artを紹介します。このコードは、3次元放射伝達コードMCFOSTに組み込まれており、MCFOSTのほとんどのモジュールと互換性があります。このコードは汎用性があり、星の近接環境を3次元でモデル化するように設計されています。方法です。本コードは、MALI（Multilevel Accelerated Lambda Iteration）法を用いて、統計的平衡方程式および放射伝達方程式を解きます。MCFOST-artは、恒星の光球の球対称モデルと、太陽大気の標準モデルでテストされました。原子レベルのポピュレーションと外向きのフラックスを計算し、これらの値をTURBOspectrumとRHコードの結果と比較しました。また、大気の膨張と回転を含む計算も行いました。純粋な局所熱力学的平衡問題と非平衡問題の両方をテストした。結果は以下の通りです。すべてのケースにおいて，すべてのコードからの結果は，すべての波長において数パーセント以内で一致し，RHとMCFOST-artの間ではサブパーセントのレベルに達した。MCFOST-artとTURBOspectrumの間には、いくつかの重要な波長領域におけるバックグラウンドオパシティの異なる処理の結果として、いくつかの限界的な不一致が残っている。

* https://arc.aiaa.org/doi/abs/10.2514/6.2021-1327

Discretization Error Estimation Using the Unsteady Error Transport Equations
Hongyu Wang, Weicheng Xue and Christopher J. Roy
数値流体力学（CFD）は，複雑な特徴を持つ流体の流れについて有意義な情報を提供することができます．しかし，慎重に特性を把握しなければ，CFD シミュレーションにおける誤差が誤った解析につながる可能性があります．本研究では，数値誤差の一種である離散化誤差の推定に焦点を当てています．この研究では，数値誤差の一種である離散化誤差の推定に焦点を当てており，これまでに開発した定常誤差輸送方程式（ETE）の手法を非定常ETEにも適用できるように拡張しています．この作業はCFDコードSENSEIに実装されています。定常ETEは、定常解の離散化誤差推定値を得るために、解が収束した後に一度だけ解かれます。非定常 ETE は原始解と一緒に進める必要があり、切り捨て誤差の時間項の推定に使用されるステンシルサイズに基づいて、原始解から一定の時間ステップ遅れて実行されます。精度の検討を行うために，厳密な解が得られる非定常テストケースを選択しました．オイラー方程式の厳密解である2次元の対流渦は，選択されたテストケースの1つである．粘性流のテストケースとして、層流ナビエ・ストークス方程式のCTS（Cross-Term Sinusoidal）製造解を用いた。補正された解の収束率を調べ、離散化誤差の推定値の精度を評価した。滑らかなテストケースでは，補正された解は原始解よりも高次で厳密解に近づくことが期待される．現在の結果では、高次の収束率が観察されているが、テストしたすべてのグリッドレベルではない。

* https://arc.aiaa.org/doi/abs/10.2514/6.2021-0170

Helicopter Rotor Optimization via Operator Overloading-Based Discrete Adjoint Approach
Reza Djeddi and Kivanc Ekici
ホバリング中のヘリコプターのローターブレードの空力設計を最適化するために、メモリ効率の良いフレームワークを開発しました。このフレームワークは、FDOTと呼ばれる完全に自動化された離散アドジョイントツールボックスに基づいています。このツールボックスは、感度や勾配の情報を非常に正確に計算することができ、メモリと計算の効率化のために独自の式テンプレートベースのアプローチを活用した演算子オーバーロード技術を使用していますが、ユーザーの介入を最小限に抑えて完全に自動化されています。本研究の主な目的は、メリットのあるヘリコプターの回転翼を「設計」することです。そこで，回転参照枠を用いたプライマリソルバーとアドジョイントソルバーを検証するために，非浮上時と浮上時のホバリング状態におけるCaradonna-Tung ローター周りの流れを調べました．最適化フレームワークの有効性は，垂直軸型風力タービン（VAWT）の構成に似た回転するNACA 0012翼の抗力最小化で初めて実証された．最後に，Caradonna-Tung ローターのシングルポイントおよびマルチポイント設計最適化の結果を示します．なお，現在の手法（FDOT）は，CREATE-AVの一部であるHelios計算プラットフォームの他の既存コードと「ブラックボックス」方式で直接結合できることに留意する必要があります．

* https://link.springer.com/chapter/10.1007/978-3-030-64514-4_10

SODA: A Serial Fortran Library for Adaptation of Structured Meshes
B. A. RobbinsEmail authorD. V. Griffiths
本講演では、軟弱地盤上の建設を支援するためのジオシンセティックスの使用に関する過去数十年の進歩を検証します。泥炭からレートに敏感な軟らかい粘土やシルトまで、さまざまな軟弱地盤を考慮しています。比較的弾性のある補強材とレートに敏感な補強材の両方を検討しています。基底部の補強、プレハブの垂直ドレーン、補強材や杭などの他のサポートを備えた堤防などを考慮しています。特に、2002年にシニア・オーサーが行ったGiroud講演以降の進歩に重点を置いています。

* https://www.aanda.org/articles/aa/pdf/forth/aa39697-20.pdf 

Atomic line radiative transfer with MCFOST
B. Tessore, C. Pinte, J. Bouvier, and F. Ménard
目的 多階層原子システムのための新しい非局所熱力学的平衡放射伝達ソルバーであるMCFOST-artを紹介します。
このコードは、3次元放射伝達コードMCFOSTに組み込まれており、MCFOSTのほとんどのモジュールと互換性があります。このコードは
汎用性があり、星の近接環境を3Dでモデル化するように設計されています。
方法です。本コードでは、統計的平衡方程式および放射伝達方程式を、Multilevel Accelerated Lambda
イテレーション法を用いて統計的平衡方程式と放射伝達方程式を解きます。MCFOST-artは、恒星の光球の球対称モデルと、太陽大気の標準モデルでテストされました。
テストしました。原子レベルの集団と外向きのフラックスを計算し、これらの値を、TURBOspectrumとRHコードの結果と比較しました。
TURBOspectrumとRHコードの結果と比較しました。また、大気の膨張と回転を含む計算も行いました。テストしたのは
純粋な局所熱力学的平衡問題と非平衡問題の両方をテストした。
結果は以下の通りです。すべての場合において，すべてのコードからの結果は，すべての波長において数パーセント以内で一致し，次の2つのコードの間ではサブパーセントレベルに達している。
RHとMCFOST-artの間でサブパーセントレベルに達した。MCFOST-artとTURBOspectrumの間には、いくつかの波長で背景不透明度の処理が異なるために、いくつかの限界的な不一致があることに注意してください。
MCFOST-artとTURBOspectrumの間には，いくつかの重要な波長領域で背景の不透明性の処理が異なるため，わずかな不一致が残っている。